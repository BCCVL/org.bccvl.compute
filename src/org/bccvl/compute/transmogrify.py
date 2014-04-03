from datetime import datetime
from itertools import chain
import mimetypes
import os.path
import glob
import re
from collective.transmogrifier.interfaces import ISectionBlueprint
from collective.transmogrifier.interfaces import ISection
from zope.interface import implementer, provider
from org.bccvl.site.namespace import BCCPROP, BCCVOCAB, BIOCLIM, DWC, BCCEMSC, BCCGCM
from org.bccvl.site.content.interfaces import IProjectionExperiment, ISDMExperiment
from gu.plone.rdf.namespace import CVOCAB
from ordf.graph import Graph
from ordf.namespace import DC
from rdflib import RDF, URIRef, Literal
from plone.i18n.normalizer.interfaces import IFileNameNormalizer
from zope.component import getUtility
from Products.CMFCore.utils import getToolByName
from zipfile import ZipFile, ZIP_DEFLATED
from plone.app.uuid.utils import uuidToObject
from gu.z3cform.rdf.interfaces import IGraph
import logging

LOG = logging.getLogger(__name__)


def guess_mimetype(name, mtr=None):
    # 1. try mimetype registry
    name = os.path.basename(name)
    mtype = None
    if mtr is not None:
        mtype = mtr.lookupExtension(name)
        if mtype is not None:
            return mtype.normalized()
        # TODO: maybe try mtr(filecontents) to use MTRs mime magic
    if mtype is None:
        mtype = mimetypes.guess_type(name)
        # TODO: add mime magic here
        # https://github.com/ahupp/python-magic/blob/master/magic.py
        if mtype is not (None, None):
            return mtype[0]
    return 'application/octet-stream'


def addLayerInfo(graph, experiment):
    for layer in set(chain(*experiment.environmental_datasets.values())):
        graph.add((graph.identifier, BIOCLIM['bioclimVariable'], layer))


def addSpeciesInfo(graph, experiment):
    if ISDMExperiment.providedBy(experiment):
        spds = uuidToObject(experiment.species_occurrence_dataset)
    if IProjectionExperiment.providedBy(experiment):
        spds = uuidToObject(experiment.species_distribution_models)
    spmd = IGraph(spds)
    for prop in (DWC['scientificName'],
                 DWC['taxonID'],
                 DWC['vernacularName']):
        val = spmd.value(spmd.identifier, prop)
        if val:
            graph.add((graph.identifier, prop, val))


@provider(ISectionBlueprint)
@implementer(ISection)
class ResultSource(object):

    def __init__(self, transmogrifier, name, options, previous):
        self.transmogrifier = transmogrifier
        self.context = transmogrifier.context
        self.name = name
        self.options = options
        self.previous = previous
        self.outputmap = options.get('outputmap')

        self.path = options['path'].strip()
        if self.path is None or not os.path.isdir(self.path):
            raise Exception(
                'Directory ({}) does not exists.'.format(str(self.path)))

        # add path prefix to imported content
        self.prefix = options.get('prefix', '').strip().strip(os.sep)
        # keys for sections further down the chain
        self.pathkey = options.get('path-key', '_path').strip()
        self.fileskey = options.get('files-key', '_files').strip()

    def createItem(self, fname, info):
        # fname: full path to file
        # info: 'title', 'type'
        name = os.path.basename(fname)
        mtr = getToolByName(self.context, 'mimetypes_registry')
        normalizer = getUtility(IFileNameNormalizer)

        datasetid = normalizer.normalize(name)

        rdf = Graph()
        rdf.add((rdf.identifier, DC['title'], Literal(name)))
        rdf.add((rdf.identifier, RDF['type'], CVOCAB['Dataset']))
        genre = info.get('genre', None)

        if genre:
            genreuri = BCCVOCAB[genre]
            rdf.add((rdf.identifier, BCCPROP['datagenre'], genreuri))
            # FIXME: attach species data to everything?

            #  resolution, toolkit, species, layers
            #  future: year, emsc, gcm
            if genreuri == BCCVOCAB['DataGenreSDMModel']:
                addLayerInfo(rdf, self.context.__parent__)
                rdf.add((rdf.identifier, BCCPROP['resolution'], self.context.resolution))
                # add species info
                addSpeciesInfo(rdf, self.context.__parent__)
            elif genreuri == BCCVOCAB['DataGenreFP']:
                rdf.add((rdf.identifier, BCCPROP['resolution'], self.context.resolution))
                addSpeciesInfo(rdf, self.context.__parent__)

                # FIXME: find a cleaner way to attach metadata
                filename = os.path.basename(fname)
                m = re.match(r'^(.*)_(.*)_(\d*)\.tif$', filename)
                if m:
                    rdf.add((rdf.identifier, BCCPROP['emissionscenario'], BCCEMSC[m.group(1)]))
                    rdf.add((rdf.identifier, BCCPROP['gcm'], BCCGCM[m.group(2)]))
                    year = Literal("start=%s; end=%s; scheme=W3C-DTF;" % (m.group(3), m.group(3)),
                                   datatype=DC['Period'])
                    rdf.add((rdf.identifier, DC['temporal'], year))
                else:
                    LOG.fatal('filename %s did not match regexp.', filename)
                # exp.future_climate_datasets()

        mimetype = info.get('mimetype', None)
        if mimetype is None:
            mimetype = guess_mimetype(fname, mtr)
        if mimetype is not None:
            rdf.add((rdf.identifier, DC['format'], Literal(mimetype)))

        LOG.info("Ingest item: %s, %s", datasetid, name)
        return {
            '_path': datasetid,
            '_type': 'org.bccvl.content.dataset',
            'title': unicode(name),
            'file': {
                'file': name,
                'contentype': mimetype,
                'filename': name
            },
            '_rdf': {
                'file': 'rdf.ttl',
                'contenttype': 'text/turtle',
            },
            '_files': {
                name: {
                    'data': open(fname).read()
                },
                'rdf.ttl': {
                    'data': rdf.serialize(format='turtle')
                }
            }
        }

    def __iter__(self):
        # exhaust previous iterator
        for item in self.previous:
            yield item

        # build list of available files
        filelist = set()
        for root, dirs, files in os.walk(self.path):
            for name in files:
                filelist.add(os.path.join(self.path, root, name))

        # import defined files:
        for fileglob, filedef in self.outputmap.get('files', {}).items():
            for fname in glob.glob(os.path.join(self.path, fileglob)):
                item = self.createItem(fname, filedef)
                yield item
                filelist.discard(fname)

        # import archives
        for archname, archdef in self.outputmap.get('archives', {}).items():
            # create archive
            farchname = os.path.join(self.path, archname)
            with ZipFile(farchname, 'w', ZIP_DEFLATED) as zipf:
                for fileglob in archdef['files']:
                    absglob = os.path.join(self.path, fileglob)
                    for fname in glob.glob(absglob):
                        zipf.write(fname, os.path.relpath(fname, self.path))
                        # discard all archived file from filelist
                        filelist.discard(fname)
            # create item of archive
            item = self.createItem(farchname, archdef)
            yield item

        # still something left?
        LOG.info("check file names: %s", ", ".join(files))
        for fname in filelist:
            LOG.info("Importing undefined item %s", fname)
            item = self.createItem(fname, {})
            # TODO: what output info do I want here?
            yield item
