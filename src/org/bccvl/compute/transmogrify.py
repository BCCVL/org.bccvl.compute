from itertools import chain
from decimal import Decimal, InvalidOperation
import mimetypes
import tempfile
import os.path
import re
from shutil import copyfileobj
from collective.transmogrifier.interfaces import ISectionBlueprint
from collective.transmogrifier.interfaces import ISection
from zope.interface import implementer, provider
from org.bccvl.site.interfaces import IBCCVLMetadata
from org.bccvl.site.content.interfaces import (IProjectionExperiment,
                                               ISDMExperiment)
from plone.i18n.normalizer.interfaces import IFileNameNormalizer
from zope.component import getUtility
from Products.CMFCore.utils import getToolByName
from plone.app.uuid.utils import uuidToObject
from urlparse import urlsplit
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


def addLayersUsedInfo(bccvlmd, result):
    layers_used = set()
    if 'environmental_datasets' in result.job_params:
        for layer in chain(*result.job_params['environmental_datasets'].values()):
            layers_used.add(layer)
    elif 'future_climate_datasets' in result.job_params:
        for layer in chain(*result.job_params['future_climate_datasets'].values()):
            layers_used.add(layer)
    bccvlmd['layers_used'] = tuple(layers_used.union(bccvlmd.get('layers_used',())))


def addSpeciesInfo(bccvlmd, result):
    if ISDMExperiment.providedBy(result.__parent__):
        spds = uuidToObject(result.job_params['species_occurrence_dataset'])
    if IProjectionExperiment.providedBy(result.__parent__):
        spds = uuidToObject(result.job_params['species_distribution_models'])
    speciesmd = IBCCVLMetadata(spds).get('species', None)
    if speciesmd:
        bccvlmd['species'] = speciesmd.copy()


@provider(ISectionBlueprint)
@implementer(ISection)
class ResultSource(object):
    # FIXME: update to process passed in dict from worker
    #        get rid of filematadata steps etc...

    def __init__(self, transmogrifier, name, options, previous):
        self.transmogrifier = transmogrifier
        self.context = transmogrifier.context
        self.name = name
        self.options = options
        self.previous = previous
        self.items = options.get('items')

        self.path = options['path'].strip()
        # if self.path is None or not os.path.isdir(self.path):
        #     raise Exception(
        #         'Directory ({}) does not exists.'.format(str(self.path)))

        # add path prefix to imported content
        self.prefix = options.get('prefix', '').strip().strip(os.sep)
        # keys for sections further down the chain
        self.pathkey = options.get('path-key', '_path').strip()
        self.fileskey = options.get('files-key', '_files').strip()


    def createItem(self, import_item):
        # fname: full path to file
        # info: 'title', 'type'
        url = import_item['file']['url']
        name = import_item['file']['filename']
        # mtr = getToolByName(self.context, 'mimetypes_registry')
        normalizer = getUtility(IFileNameNormalizer)
        datasetid = normalizer.normalize(name)

        bccvlmd = import_item['bccvlmetadata']

        genre = bccvlmd.get('genre', None)
        if genre:
            # FIXME: attach species data to everything?
            if genre in ('DataGenreSDMModel', 'DataGenreCP', 'DataGenreClampingMask'):
                addLayersUsedInfo(bccvlmd, self.context)
                bccvlmd['resolution'] = self.context.job_params['resolution']
                addSpeciesInfo(bccvlmd, self.context)
            if genre == 'DataGenreEnsembleResult':
                bccvlmd['resolution'] = self.context.job_params['resolution']
            elif genre == 'DataGenreFP':
                addLayersUsedInfo(bccvlmd, self.context)
                bccvlmd['resolution'] = self.context.job_params['resolution']
                addSpeciesInfo(bccvlmd, self.context)
                # exp.future_climate_datasets()
            if genre == 'DataGenreSDMEval' and bccvlmd.get('thresholds', None):  # only for mimetype text/csv
                # convert thresholds to Decimal
                for key, value in bccvlmd['thresholds'].items():
                    try:
                        bccvlmd['thresholds'][key] = Decimal(value)
                    except (TypeError, InvalidOperation) as e:
                        LOG.warn("Couldn't parse threshold vlaue '%s' (%s) from file '%s': %s", name, key, url, repr(e))

        mimetype = import_item['file']['contenttype']
        # mimetype = info.get('mimetype', None)
        # if mimetype is None:
        #     mimetype = guess_mimetype(fname, mtr)

        # build item
        item = {
            '_path': datasetid,
            'title': unicode(name),
            'description': import_item.get('title', u''),
            'bccvlmetadata': bccvlmd,
            '_layermd': import_item.get('layermd', {})
        }

        # dataset or remotedataset?
        urlparts = urlsplit(import_item['file']['url'])  # params resultsfolder
        # TODO decide local remote here or use import_item['file']['url']
        if 'http' in urlparts.scheme:
            # remote
            remoteurl = re.sub(r'^swift\+', '', import_item['file']['url'])
            item.update({
                '_type': 'org.bccvl.content.remotedataset',
                'remoteUrl': remoteurl,
                # FIXME: hack to pass on content type to FileMetadataToBCCVL blueprint
                '_files': {
                    remoteurl: {
                        'contenttype': mimetype,
                    }
                },
                '_filemetadata': {
                    # key relates to 'remoteUrl'
                    remoteurl: import_item.get('filemetadata', {}) or {},  # make sure it's not none
                }
            })
        else:
            # assume local storage
            item.update({
                '_type': 'org.bccvl.content.dataset',
                'file': {
                    'file': name,
                    'contenttype': mimetype,
                    'filename': name
                },
                '_files': {
                    name: {
                        'name': name,
                        'path': urlparts.path,
                        'data': open(urlparts.path, 'r')
                    }
                },
                '_filemetadata': {
                    # key relates to 'file':'file'
                    name: import_item.get('filemetadata', {}) or {},  # make sure it's not none
                }
            })

        LOG.info("Ingest item: %s, %s", datasetid, name)
        return item

    def __iter__(self):
        # exhaust previous iterator
        for item in self.previous:
            yield item

        # Import each item
        for import_item in self.items:
            item = self.createItem(import_item)
            yield item


# TODO: this step will disappear, once background workers do the
#       metadata extraction
#       also the metadata generated here should be produced somewhere else
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

    def _place_file_on_filesystem(self, fileitem):
        _, ext = os.path.split(fileitem['name'])
        # Add suffix to temp file to keep gdal's VSI happy
        tmpfd, tmpname = tempfile.mkstemp(suffix=ext)
        tmpfile = os.fdopen(tmpfd, 'w')
        try:
            copyfileobj(fileitem['data'], tmpfile)
        except AttributeError:
            # we get an AttributeError in case data is not a file like object
            # let's try to write data directly
            tmpfile.write(fileitem['data'])
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

            fileid = (item.get('remoteUrl') or
                      item.get('file', {}).get('file'))
            if not fileid:
                # nothing to inspect
                yield item
                continue

            # check if we have a file
            files = item.setdefault(self.fileskey, {})
            if not files:
                # we have a file but no data for it
                yield item
                continue
            fileitem = files.get(fileid)
            if not fileitem:
                # our file is not in the _files list
                yield item
                continue
            # get content type and filename
            filect = fileitem.get('contenttype') or item.get('file', {}).get('contenttype')
            filename = fileitem.get('filename') or item.get('file', {}).get('filename')
            # get path to data on local filesystem
            # FIXME: data might be file like object
            if 'path' in fileitem:
                filepath = fileitem['path']
            elif 'data' in fileitem:
                tmpfile = self._place_file_on_filesystem(fileitem)
                filepath = tmpfile


            # finally:
            #     if tmpfile:
            #         os.unlink(tmpfile)
            if tmpfile:
                os.unlink(tmpfile)
            yield item
