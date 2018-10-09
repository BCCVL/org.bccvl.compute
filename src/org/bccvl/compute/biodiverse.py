from copy import deepcopy
from decimal import Decimal
import logging
from pkg_resources import resource_string

from plone import api
# do this dynamically in site module?
from zope.interface import provider

from org.bccvl.compute.utils import getdatasetparams
from org.bccvl.site.interfaces import IComputeMethod
from org.bccvl.site.utils import get_results_dir
from org.bccvl.tasks.compute import perl_task
from org.bccvl.tasks.plone import after_commit_task, HIGH_PRIORITY


LOG = logging.getLogger(__name__)


def get_biodiverse_params(result):
    # make a deep copy of the params to not accedientially modify the
    # persisted dict
    params = deepcopy(result.job_params)

    # params['projections'] is a list of dicts with 'threshold' and 'uuid'
    # and sholud become a list of dicts with datasetinfos + threshold?

    dslist = []
    for dsparam in params['projections']:
        dsinfo = getdatasetparams(dsparam['dataset'])
        dsinfo['threshold'] = dsparam['threshold']
        dslist.append(dsinfo)
    # replace projections param
    params['projections'] = dslist

    # TODO: quick fix Decimal json encoding through celery (where is my custom
    # json encoder gone?)
    # -> problem is oslo jsonutils, whihch patches anyjson with it's own
    #    loads/dumps methods.
    # we would normally use simplejson, which supports decimal, but oslo
    # patches it in a way so that decimal no longer works
    for key, item in params.items():
        if isinstance(item, Decimal):
            params[key] = float(item)
    # ptach threshold vasue as well
    for pds in params['projections']:
        thresholds = pds['threshold']
        for key, item in thresholds.items():
            if isinstance(item, Decimal):
                thresholds[key] = float(item)

    workerhints = {
        'files': ('projections', )
    }
    return {'env': {}, 'params': params, 'worker': workerhints}


def generate_biodiverse_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/biodiverse.pl'),
    ])
    return script


OUTPUTS = {
    'files': {
        '*ENDW_SINGLE.tif': {
            "skip": True
        },
        '*REDUNDANCY_SET1.tif': {
            "skip": True
        },
        '*.plout': {
            "title": "Log file",
            "genre": "DataGenreLog",
            "mimetype": "text/plain",
            "order": 9
        },
        'proj_*.tif': {
            "title": "Biodiverse input",
            "genre": "DataGenreBinaryImage",
            "mimetype": "image/geotiff",
            "order": 10
        },
        '*ENDW_CWE.tif': {
            "title": "Endemism - corrected weighted",
            "genre": "DataGenreENDW_CWE",
            "mimetype": "image/geotiff",
            "order": 2
        },
        '*ENDW_WE.tif': {
            "title": "Endemism - weighted",
            "genre": "DataGenreENDW_WE",
            "mimetype": "image/geotiff",
            "order": 3
        },
        '*ENDW_RICHNESS.tif': {
            "title": "Species Richness",
            "genre": "DataGenreENDW_RICHNESS",
            "mimetype": "image/geotiff",
            "order": 1
        },
        '*REDUNDANCY_SET2.tif': {
            "title": "Redundancy - neigbour set 2",
            "genre": "DataGenreREDUNDANCY_SET2",
            "mimetype": "image/geotiff",
            "order": 20
        },
        '*REDUNDANCY_ALL.tif': {
            "title": "Redundancy - both neigbour sets",
            "genre": "DataGenreREDUNDANCY_ALL",
            "mimetype": "image/geotiff",
            "order": 20
        },
        '*RAREW_CWE.tif': {
            "title": "Rarity - corrected weighted",
            "genre": "DataGenreRAREW_CWE",
            "mimetype": "image/geotiff",
            "order": 4
        },
        '*RAREW_RICHNESS.tif': {
            "title": "Rarity whole - Richness used in RAREW_CWE",
            "genre": "DataGenreRAREW_RICHNESS",
            "mimetype": "image/geotiff",
            "skip": True
        },
        '*RAREW_WE.tif': {
            "title": "Rarity - weighted",
            "genre": "DataGenreRAREW_WE",
            "mimetype": "image/geotiff",
            "order": 5
        },
        '*.csv': {
            "title": "Biodiverse spatial analysis result",
            "genre": "DataGenreBiodiverseOutput",
            "mimetype": "text/csv",
            "order": 6
        },
        '*.bds': {
            # Perl Storable package
            'title': 'Biodiverse output',
            'genre': 'DataGenreBiodiverseModel',
            'mimetype': 'application/octet-stream',
            "order": 7
        },
        '*.pl': {
            'title': 'Job Script',
            'genre': 'JobScript',
            'mimetype': 'application/x-perl',
            "order": 8
        }
    }
}


@provider(IComputeMethod)
def execute(result, toolkit, priority=HIGH_PRIORITY):
    """
    This function takes an experiment and executes.

    It usesenvirnoment variables WORKER_DIR or HOME as root folder to execute
    experiments.

    After the execution finishes the output files will be attached to the
    experiment.

    :param experiment: The experiment holding the configuration and receiving
                       the results
    :type experiment: org.bccvl.site.content.IExperiment


    """
    # FIXME: biodiverse is not yet a content based toolkit
    # try:
    #     OUTPUTS = json.loads(toolkit.output)
    # except (ValueError, TypeError) as e:
    #     LOG.fatal("couldn't load OUTPUT form toolkit %s: %s",
    #               toolkit.getId(), e)
    #     OUTPUTS = {}
    params = get_biodiverse_params(result)
    script = generate_biodiverse_script()
    member = api.user.get_current()
    context = {
        'context': '/'.join(result.getPhysicalPath()),
        'user': {'id': member.getUserName(),
                 'email': member.getProperty('email'),
                 'fullname': member.getProperty('fullname')
                 },
        'experiment': {'title': result.__parent__.title,
                       'url': result.__parent__.absolute_url()
                       }
    }
    params['result'] = {
        'results_dir': get_results_dir(result, result.REQUEST),
        'outputs': OUTPUTS
    }
    params['worker']['script'] = {
        'name': 'biodiverse.pl',
        'script': script,
    }
    # set debug flag
    params['worker']['zipworkenv'] = api.env.debug_mode()
    # send job to queue
    after_commit_task(perl_task, priority, params, context)
