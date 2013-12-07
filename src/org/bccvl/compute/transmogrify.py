from datetime import datetime
import mimetypes
import os.path
import glob
from collective.transmogrifier.interfaces import ISectionBlueprint
from collective.transmogrifier.interfaces import ISection
from zope.interface import implementer, provider
from org.bccvl.site.namespace import BCCPROP, BCCVOCAB, BIOCLIM
from gu.plone.rdf.namespace import CVOCAB
from ordf.graph import Graph
from ordf.namespace import DC
from rdflib import RDF, URIRef, Literal
from plone.i18n.normalizer.interfaces import IFileNameNormalizer
from zope.component import getUtility
from Products.CMFCore.utils import getToolByName
from zipfile import ZipFile, ZIP_DEFLATED
import logging

LOG = logging.getLogger(__name__)


# register a few mimetypes
mimetypes.add_type('text/plain', '.rascii')
mimetypes.add_type('text/plain', '.rout')
mimetypes.add_type('application/octet-stream', '.rdata')
mimetypes.add_type('image/geotiff', '.geotiff')


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


GENRE_MAP = {
    None: BCCVOCAB['DataGenreUnknown'],
    'eval':  BCCVOCAB['DataGenreSDMEval'],
    'model':  BCCVOCAB['DataGenreSD'],
    'log':  BCCVOCAB['DataGenreLog'],
    'projection': BCCVOCAB['DataGenreFP']}

# TODO: replace BCCVOCAB with something standard (NFO? mime type onto?)
FORMAT_MAP = {
    None: BCCVOCAB['DataSetFormatUnknown'],
    'RData': BCCVOCAB['DataSetFormatRDATA'],
    'csv': BCCVOCAB['DataSetFormatCSV'],
    'GTiff': BCCVOCAB['DataSetFormatGTiff'],
    'txt': BCCVOCAB['DataSetFormatText'],
    'html': BCCVOCAB['DataSetFormatHTML'],
    'png': BCCVOCAB['DataSetFormatPNG'],
    'zip': BCCVOCAB['DataSetFormatZIP'], # need a way to describe contained data (if e.g. biomod sdm where everything is RData, or layers with all GTiff, asc(gz), ...)
}


def addLayerInfo(graph, experiment):
    for layer in experiment.environmental_layers.keys():
        graph.add((graph.identifier, BIOCLIM['bioclimVariable'], layer))


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

        mimetype = guess_mimetype(fname, mtr)
        datasetid = normalizer.normalize(name)

        # genre, format = get_data_genre(name)

        rdf = Graph()
        rdf.add((rdf.identifier, DC['title'], Literal(name)))
        rdf.add((rdf.identifier, RDF['type'], CVOCAB['Dataset']))
        genre = info.get('type', None)
        if genre:
            genreuri = GENRE_MAP.get(genre, None)
            if genreuri:
                rdf.add((rdf.identifier, BCCPROP['datagenre'],
                         genreuri))
            if genreuri == BCCVOCAB['DataGenreSD']:
                addLayerInfo(rdf, self.context)
        format = info.get('format', None)
        if format is not None:
            rdf.add((rdf.identifier, BCCPROP['format'], FORMAT_MAP[format]))

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
        for fileglob, filedef in self.outputmap['files'].items():
            for fname in glob.glob(os.path.join(self.path, fileglob)):
                item = self.createItem(fname, filedef)
                yield item
                filelist.discard(fname)

        # import archives
        for archname, archdef in self.outputmap['archives'].items():
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
