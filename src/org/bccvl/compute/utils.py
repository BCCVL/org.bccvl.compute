"""
.. module:: utils
   :synopsis: Various little helper functions to be re-used within this
              package.

.. moduleauthor:: Gerhard Weis <g.weis@griffith.edu.au>
"""
from decimal import Decimal
import logging
import os
import pwd
import socket
import tempfile

from plone.app.uuid.utils import uuidToObject
from plone.registry.interfaces import IRegistry
from plone.uuid.interfaces import IUUID
from zope.component import getUtility

from org.bccvl.site.api.dataset import getdsmetadata
from org.bccvl.site.interfaces import IDownloadInfo, IBCCVLMetadata
from org.bccvl.site.swift.interfaces import ISwiftSettings


LOG = logging.getLogger(__name__)


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
    species = dsmdr.get('species', {}).get('scientificName')
    if species:
        dsinfo['species'] = species
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


# FIXME: no longer needed?
def get_public_ip():
    # check if the environment variable EXT_IP has some useful value
    ip = os.environ.get('EXT_IP', None)
    if ip:
        return ip
    # otherwise we connect to some host, and check which local ip the socket uses
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.connect(('google.com', 80))
        # TODO: could do name lookup with socket.gethostbyaddr('ip')[0]
        #       or socket.getnameinfo(s.getsockname())[0]
        #       namelookup may throw another exception?
        return s.getsockname()[0]
    except Exception as e:
        LOG.warn("couldn't connect to google.com: %s", repr(e))
    # we still have no clue, let's try it via hostname
    try:
        return socket.gethostbyname(socket.gethostname())
    except Exception as e:
        LOG.warn("couldn't resolve '%s': %s", socket.gethostname(), repr(e))
    # last chance
    return socket.getfqdn()


def get_hostname(request):
    """ Extract hostname in virtual-host-safe manner

    @param request: HTTPRequest object, assumed contains environ dictionary

    @return: Host DNS name, as requested by client. Lowercased, no port part.
             Return None if host name is not present in HTTP request headers
             (e.g. unit testing).
    """

    if "HTTP_X_FORWARDED_HOST" in request.environ:
        # Virtual host
        host = request.environ["HTTP_X_FORWARDED_HOST"]
    elif "HTTP_HOST" in request.environ:
        # Direct client request
        host = request.environ["HTTP_HOST"]
    else:
        return None

    # separate to domain name and port sections
    host=host.split(":")[0].lower()

    return host


def get_username():
    return pwd.getpwuid(os.getuid()).pw_name


def get_results_dir(result, request):
    swiftsettings = getUtility(IRegistry).forInterface(ISwiftSettings)
    if swiftsettings.storage_url:
        results_dir = 'swift+{storage_url}/{container}/{path}/'.format(
            storage_url=swiftsettings.storage_url,
            container=swiftsettings.result_container,
            path=IUUID(result)
        )
    else:
        # if swift is not setup we use local storage
        results_dir = 'scp://{uid}@{ip}{path}/'.format(
            uid=pwd.getpwuid(os.getuid()).pw_name,
            # FIXME: hostname from request is not good enough...
            #        need to get ip or host from registry
            #        (is ok for testing)
            #ip=get_public_ip(),
            ip=get_hostname(request),
            path=tempfile.mkdtemp(prefix='result_import_')
        )
    return results_dir
