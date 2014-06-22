"""
.. module:: utils
   :synopsis: Various little helper functions to be re-used within this
              package.

.. moduleauthor:: Gerhard Weis <g.weis@griffith.edu.au>
"""
import os
import os.path
from decimal import Decimal
from plone.app.uuid.utils import uuidToObject
from org.bccvl.site.api.dataset import getbiolayermetadata
from gu.z3cform.rdf.interfaces import IGraph
from org.bccvl.site.namespace import DWC
from org.bccvl.site.interfaces import IDownloadInfo

import logging
LOG = logging.getLogger(__name__)

DATA_MOVER = 'http://127.0.0.1:10700/data_mover'
COMPUTE_HOST = 'localhost'
COMPUTE_USER = 'bccvl'


def decimal_encoder(o):
    if isinstance(o, Decimal):
        return float(o)
    raise TypeError(repr(o) + " is not JSON serializable")


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
        'internalurl':  info['alturl'][0]
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
    dsmd = IGraph(dsobj)
    species = dsmd.value(dsmd.identifier, DWC['scientificName'])
    if species:
        dsinfo['species'] = unicode(species)
    # if we can get layermetadata, let's add it
    biomod = getbiolayermetadata(dsobj)
    if biomod:
        dsinfo['layers'] = dict(((k, v['filename']) for
                                 k, v in biomod.items()))
        # FIXME: get type from metadata
        dsinfo['type'] = 'continuous'
    # return infoset
    return dsinfo
