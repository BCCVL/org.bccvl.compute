"""
.. module:: utils
   :synopsis: Various little helper functions to be re-used within this
              package.

.. moduleauthor:: Gerhard Weis <g.weis@griffith.edu.au>
"""
import logging

from plone.app.uuid.utils import uuidToObject

from org.bccvl.site.api.dataset import getdsmetadata
from org.bccvl.site.interfaces import IDownloadInfo, IBCCVLMetadata


LOG = logging.getLogger(__name__)


# TODO: use getDatasetMetadata from xmlrpc package. (remove
#       getbiolayermetadata in getExperimentInfo)
def getDatasetInfo(datasetitem, uuid):
    # extract various infos for a dateset
    #  filename
    #  downloadurl
    #
    #  internal_url
    #######
    #  species

    # get filename
    info = IDownloadInfo(datasetitem)
    return {
        'uuid': uuid,
        'filename': info['filename'],
        'downloadurl': info['url'],
    }


def getdatasetparams(uuid):
    # return dict with:
    #    filename
    #    downloadurl
    #    dm_accessurl-> maybe add url rewrite to datamover?
    #    # occurrence specific:
    #    species
    #    # raster specific:
    #    layers ... need to split this up
    dsobj = uuidToObject(uuid)
    if dsobj is None:
        return None
    dsinfo = getDatasetInfo(dsobj, uuid)
    # if we have species info add it

    dsmdr = IBCCVLMetadata(dsobj)
    species = dsmdr.get('species', {})
    commonNames = ''
    if isinstance(species, (list, tuple)):
        # We have a list of species dicts....
        commonNames = [s['vernacularName']
                   for s in species if s.get('vernacularName')]
        species = [s['scientificName']
                   for s in species if s.get('scientificName')]
    else:
        commonNames = species.get('vernacularName', '')
        species = species.get('scientificName')
    if species:
        dsinfo['species'] = species
        dsinfo['commonName'] = commonNames
    # if we can get layermetadata, let's add it
    biomod = getdsmetadata(dsobj)
    layers = biomod.get('layers', [])

    if len(layers) > 0:
        for lk, lv in biomod['layers'].items():
            if lv is not None:
                dsinfo.setdefault('layers', {})[lk] = {
                    'filename': lv.get('filename', biomod['filename']),
                    'datatype': lv.get('datatype', None)}
    # return infoset
    return dsinfo
