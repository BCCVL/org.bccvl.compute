from datetime import datetime
import mimetypes
import os.path
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
        # TODO: add mime magic here https://github.com/ahupp/python-magic/blob/master/magic.py
        if mtype is not (None, None):
            return mtype[0]
    return 'application/octet-stream'


def get_data_genre(fname):
    fname = fname.lower()
    if fname.endswith('.rdata'):
        if 'eval' in fname:
            return BCCVOCAB['DataGenreSDMEval'], BCCVOCAB['DataSetFormatRDATA']
        return BCCVOCAB['DataGenreSD'], BCCVOCAB['DataSetFormatRDATA']
    if fname.endswith('.csv'):
        return BCCVOCAB['DataGenreSDMEval'], BCCVOCAB['DataSetFormatCSV']
    if fname.endswith('.tif'):
        return BCCVOCAB['DataGenreFP'], BCCVOCAB['DataSetFormatGTiff']
    if fname.endswith('.rout'):
        return BCCVOCAB['DataGenreLog'], BCCVOCAB['DataSetFormatText']
    if fname.endswith('.png'):
        return BCCVOCAB['DataGenreSDMEval'], BCCVOCAB['DataSetFormatPNG']
    return None, None


@provider(ISectionBlueprint)
@implementer(ISection)
class ResultSource(object):

    def __init__(self, transmogrifier, name, options, previous):
        self.transmogrifier = transmogrifier
        self.context = transmogrifier.context
        self.name = name
        self.options = options
        self.previous = previous

        self.path = options['path'].strip()
        if self.path is None or not os.path.isdir(self.path):
            raise Exception('Directory ({}) does not exists.'.format(str(self.path)))

        # add path prefix to imported content
        self.prefix = options.get('prefix', '').strip().strip(os.sep)
        # keys for sections further down the chain
        self.pathkey = options.get('path-key', '_path').strip()
        self.fileskey = options.get('files-key', '_files').strip()

    def createItem(self, fname):
        # fname: full path to file
        name = os.path.basename(fname)
        mtr = getToolByName(self.context, 'mimetypes_registry')
        normalizer = getUtility(IFileNameNormalizer)

        mimetype = guess_mimetype(fname, mtr)
        datasetid = normalizer.normalize(name)

        genre, format = get_data_genre(name)

        rdf = Graph()
        rdf.add((rdf.identifier, DC['title'], Literal(name)))
        rdf.add((rdf.identifier, RDF['type'], CVOCAB['Dataset']))
        if genre is not None:
            rdf.add((rdf.identifier, BCCPROP['datagenre'], genre))
        if format is not None:
            rdf.add((rdf.identifier, BCCPROP['format'], format))

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

        # start our own source
        for root, dirs, files in os.walk(self.path):
            # use: del dirs['name'] to avoid traversing through subdir 'name'
            # root is always full path underneath self.path
            LOG.info("check file names: %s", ", ".join(files))
            for name in files:
                fname = os.path.join(self.path, root, name)
                item = self.createItem(fname)
                yield item
