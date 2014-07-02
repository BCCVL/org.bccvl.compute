from itertools import chain
import mimetypes
import tempfile
import shutil
import os.path
import glob
import re
from urllib2 import urlopen
from io import BytesIO
from collective.transmogrifier.interfaces import ISectionBlueprint
from collective.transmogrifier.interfaces import ISection
from zope.interface import implementer, provider
from org.bccvl.site.namespace import (BCCPROP, BCCVOCAB, BIOCLIM,
                                      DWC, BCCEMSC, BCCGCM)
from org.bccvl.site.content.interfaces import (IProjectionExperiment,
                                               ISDMExperiment)
from gu.plone.rdf.namespace import CVOCAB
from ordf.graph import Graph
from ordf.namespace import DC
from rdflib import RDF, Literal
from plone.i18n.normalizer.interfaces import IFileNameNormalizer
from zope.component import getUtility
from Products.CMFCore.utils import getToolByName
from zipfile import ZipFile, ZIP_DEFLATED
from plone.app.uuid.utils import uuidToObject
from gu.z3cform.rdf.interfaces import IGraph
from csv import DictReader
from decimal import Decimal, InvalidOperation
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


def addSpeciesInfo(graph, result):
    if ISDMExperiment.providedBy(result.__parent__):
        spds = uuidToObject(result.job_params['species_occurrence_dataset'])
    if IProjectionExperiment.providedBy(result.__parent__):
        spds = uuidToObject(result.job_params['species_distribution_models'])
    spmd = IGraph(spds)
    for prop in (DWC['scientificName'],
                 DWC['taxonID'],
                 DWC['vernacularName']):
        val = spmd.value(spmd.identifier, prop)
        if val:
            graph.add((graph.identifier, prop, val))


def extractThresholdValues(fname):
    # parse csv file and add threshold values as dict
    # this method might be called multiple times for one item

    # There are various formats:
    #   combined.modelEvaluation: Threshold Name, Testing.data, Cutoff,
    #                             Sensitivity, Specificity
    #   biomod2.modelEvaluation: Threshold Name, Testing.data, Cutoff.*,
    #                            Sensitivity.*, Specificity.*
    #   maxentResults.csv: Species,<various columns with interesting values>
    #                <threshold name><space><cumulative threshold,
    #                              logistic threshold,area,training omission>
    # FIXME: this is really ugly and csv format detection should be done
    #        differently
    thresholds = {}
    if fname.endswith('maxentResults.csv'):
        csvfile = open(fname, 'r')
        dictreader = DictReader(csvfile)
        row = dictreader.next()
        # There is only one row in maxentResults
        namelist = (
            'Fixed cumulative value 1', 'Fixed cumulative value 5',
            'Fixed cumulative value 10', 'Minimum training presence',
            '10 percentile training presence',
            '10 percentile training presence',
            'Equal training sensitivity and specificity',
            'Maximum training sensitivity plus specificity',
            'Balance training omission, predicted area and threshold value',
            'Equate entropy of thresholded and original distributions')
        for name in namelist:
            # We extract only 'cumulative threshold'' values
            threshold = '{} cumulative threshold'.format(name)
            thresholds[threshold] = Decimal(row[threshold])
    else:
        # assume it's one of our biomod/dismo results
        csvfile = open(fname, 'r')
        dictreader = DictReader(csvfile)
        # search the field with Cutoff
        name = 'Cutoff'
        for fieldname in dictreader.fieldnames:
            if fieldname.startswith('Cutoff.'):
                name = fieldname
                break
        try:
            for row in dictreader:
                try:
                    thresholds[row['']] = Decimal(row[name])
                except (TypeError, InvalidOperation) as e:
                    LOG.warn("Couldn't parse threshold value '%s' (%s) from"
                             "file '%s': %s",
                             name, row[name], fname, repr(e))
        except KeyError:
            LOG.warn("Couldn't extract Threshold '%s' from file '%s'",
                     name, fname)
    return thresholds


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
                rdf.add((rdf.identifier, BCCPROP['resolution'],
                         self.context.resolution))
                # add species info
                addSpeciesInfo(rdf, self.context)
            elif genreuri == BCCVOCAB['DataGenreFP']:
                # TODO: resolution is also in job_params
                #       self.conetxt.job_params['resolution'] ... shall we remove the attribute?
                rdf.add((rdf.identifier, BCCPROP['resolution'],
                         self.context.resolution))
                addSpeciesInfo(rdf, self.context)

                # FIXME: find a cleaner way to attach metadata
                filename = os.path.basename(fname)
                m = re.match(r'^proj_(.*)_(.*)_(\d*)_.*\.tif$', filename)
                if m:
                    rdf.add((rdf.identifier, BCCPROP['emissionscenario'],
                             BCCEMSC[m.group(1)]))
                    rdf.add((rdf.identifier, BCCPROP['gcm'],
                             BCCGCM[m.group(2)]))
                    year = Literal("start={0}; end={0}; scheme=W3C-DTF;"
                                   .format(m.group(3)),
                                   datatype=DC['Period'])
                    rdf.add((rdf.identifier, DC['temporal'], year))
                else:
                    LOG.fatal('filename %s did not match regexp.', filename)
                # exp.future_climate_datasets()
            elif genreuri == BCCVOCAB['DataGenreSDMEval']:
                if info.get('mimetype') == 'text/csv':
                    thresholds = extractThresholdValues(fname)
                    if thresholds:
                        if 'thresholds' not in info:
                            info['thresholds'] = {}
                        info['thresholds'].update(thresholds)

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
            'description': info.get('title', u''),
            'thresholds': info.get('thresholds', None),
            'file': {
                'file': name,
                'contenttype': mimetype,
                'filename': name
            },
            '_rdf': {
                'file': 'rdf.ttl',
                'contenttype': 'text/turtle',
            },
            # FIXME: I think adding ofen file descriptors works just fine,
            #        otherwise could use new 'path' feature with additional blueprint
            '_files': {
                name: {
                    'name': name,
                    'data': open(fname).read()
                },
                'rdf.ttl': {
                    'name': 'rdf.ttl',
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
        # sort list of globs with longest first:
        globlist = sorted(self.outputmap.get('files', {}).items(),
                          key=lambda item: (-len(item[0]), item[0]))
        for fileglob, filedef in globlist:
            for fname in glob.glob(os.path.join(self.path, fileglob)):
                if fname in filelist:
                    # we import only if we haven't done so already
                    # otherwise a 2nd glob may match again
                    item = self.createItem(fname, filedef)
                    yield item
                filelist.discard(fname)

        # import archives
        for archname, archdef in self.outputmap.get('archives', {}).items():
            # create archive
            farchname = os.path.join(self.path, archname)
            empty = True
            with ZipFile(farchname, 'w', ZIP_DEFLATED) as zipf:
                for fileglob in archdef['files']:
                    absglob = os.path.join(self.path, fileglob)
                    for fname in glob.glob(absglob):
                        empty = False
                        zipf.write(fname, os.path.relpath(fname, self.path))
                        # discard all archived file from filelist
                        filelist.discard(fname)
            # create item of archive
            if not empty:
                # only import non empty zip files
                item = self.createItem(farchname, archdef)
                yield item

        # still something left?
        LOG.info("check file names: %s", ", ".join(files))
        for fname in filelist:
            LOG.info("Importing undefined item %s", fname)
            item = self.createItem(fname, {})
            # TODO: what output info do I want here?
            yield item


@provider(ISectionBlueprint)
@implementer(ISection)
class FileMetadata(object):

    """Use hachoir library to extract metadata from file and try to do.

    something meaningful with.

    metadata is stored under item["_filemetadata"] as a simple dict as
    returned by hachoir.
    """

    def __init__(self, transmogrifier, name, options, previous):
        """missing docstring."""
        self.transmogrifier = transmogrifier
        self.name = name
        self.options = options
        self.previous = previous
        self.context = transmogrifier.context

        # keys for sections further down the chain
        self.fileskey = options.get('files-key', '_files').strip()
        self.filemetadatakey = options.get('filemetadata-key',
                                           '_filemetadata').strip()

    def _place_file_on_filesystem(self, data):
        tmpfd, tmpname = tempfile.mkstemp()
        tmpfile = os.fdopen(tmpfd, 'w')
        tmpfile.write(data)
        tmpfile.close()
        # TODO: catch exception during copy and cleanup if necessary
        return tmpname

    def __iter__(self):
        """missing docstring."""        # exhaust previous iterator
        for item in self.previous:
            tmpfile = None
            # check if we have a dataset
            if item['_type'] not in ('org.bccvl.content.dataset',
                                     'org.bccvl.content.remotedataset'):
                # not a dataset
                yield item
                continue

            fileattr = (item.get('remoteUrl') or
                        item.get('file', {}).get('file'))
            if not fileattr:
                # nothing to inspect
                yield item
                continue

            # check if we have a file
            files = item.setdefault(self.fileskey, {})
            if not files:
                # we have a file but no data for it
                yield item
                continue
            fileitem = files.get(fileattr)
            if not fileitem:
                # our file is not in the _files list
                yield item
                continue
            if 'path' in fileitem:
                filepath = fileitem['path']
            elif 'data' in fileitem:
                tmpfile = self._place_file_on_filesystem(fileitem['data'])
                filepath = tmpfile
            filect = fileitem.get('contenttype') or item.get('file', {}).get('contenttype')
            filename = fileitem.get('filename') or item.get('file', {}).get('filename')

            # ok .. everything ready let's try our luck
            # fileio ... stream to read data from
            # filect ... the supposed content type of the file
            from .mdextractor import MetadataExtractor
            mdextractor = MetadataExtractor()
            # mdextractor = getUtility(IMetadataExtractor)
            try:
                md = mdextractor.from_file(filepath, filect)
                item['_filemetadata'] = {
                    filename: md
                }
            except Exception as ex:
                LOG.warn("Couldn't extract metadata from file: %s : %s",
                         filename, repr(ex))
            finally:
                if tmpfile:
                    os.unlink(tmpfile)
            yield item
