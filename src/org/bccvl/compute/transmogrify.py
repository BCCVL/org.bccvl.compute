from datetime import datetime
import mimetypes
import os.path
from collective.transmogrifier.interfaces import ISectionBlueprint
from collective.transmogrifier.interfaces import ISection
from zope.interface import implementer, provider
from org.bccvl.site.namespace import BCCPROP, BCCVOCAB
from gu.plone.rdf.namespace import CVOCAB
from ordf.graph import Graph
from ordf.namespace import DC
from rdflib import RDF, URIRef, Literal
from plone.i18n.normalizer.interfaces import IFileNameNormalizer
from zope.component import getUtility
from Products.CMFCore.utils import getToolByName



# register a few mimetypes
mimetypes.add_type('text/plain', '.rascii')
mimetypes.add_type('text/plain', '.rout')
mimetypes.add_type('application/octet-stream', '.rdata')
mimetypes.add_type('image/geotiff', '.geotiff')

def guess_mimetype(name, path='', mtr=None):
    # 1. try mimetype registry
    mtype =  None
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

    def __iter__(self):
        # exhaust previous iterator
        for item in self.previous:
            yield item

        mtr = getToolByName(self.context, 'mimetypes_registry')
        normalizer = getUtility(IFileNameNormalizer)

        # start our own source
        for fname in os.listdir(self.path):
            if os.path.isdir(os.path.join(self.path, fname)):
                # TODO: zip up folder and process zip file
                continue

            mimetype = guess_mimetype(fname, self.path, mtr)
            datasetid = normalizer.normalize(fname)

            genre, format = get_data_genre(fname)

            rdf = Graph()
            rdf.add((rdf.identifier, DC['title'], Literal(fname)))
            rdf.add((rdf.identifier, RDF['type'], CVOCAB['Dataset']))
            if genre is not None:
                rdf.add((rdf.identifier, BCCPROP['datagenre'], genre))
            if format is not None:
                rdf.add((rdf.identifier, BCCPROP['format'], format))

            item = {
                '_path': datasetid,
                '_type': 'org.bccvl.content.dataset',
                'title': unicode(fname),
                'file': {
                    'file': fname,
                    'contentype': mimetype,
                    'filename': fname
                    },
                '_rdf': {
                    'file': 'rdf.ttl',
                    'contenttype': 'text/turtle',
                    },
                '_files': {
                    fname: {
                        'data': open(os.path.join(self.path, fname)).read()
                        },
                    'rdf.ttl': {
                        'data': rdf.serialize(format='turtle')
                        }
                    }
                }
            yield item
