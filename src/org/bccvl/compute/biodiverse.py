from copy import deepcopy
import logging
from pkg_resources import resource_string

from plone import api
# do this dynamically in site module?
from zope.interface import provider

from org.bccvl.compute.utils import getdatasetparams, get_results_dir
from org.bccvl.site.interfaces import IComputeMethod
from org.bccvl.tasks.compute import perl_task
from org.bccvl.tasks.plone import after_commit_task


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
        '*.plout': {
            "title": "Log file",
            "genre": "DataGenreLog",
            "mimetype": "text/plain"
        },
        'proj_*.tif': {
            "title": "Binary input",
            "genre": "DataGenreBinaryImage",
            "mimetype": "image/geotiff"
        },
        '*ENDW_CWE.tif': {
            "title": "Endemism whole - Corrected Weighted Endemism",
            "genre": "DataGenreENDW_CWE",
            "mimetype": "image/geotiff"
        },
        '*ENDW_WE.tif': {
            "title": "Endemism whole - Weighted Endemism",
            "genre": "DataGenreENDW_WE",
            "mimetype": "image/geotiff"
        },
        '*ENDW_RICHNESS.tif': {
            "title": "Endemism whole - Richness used in ENDW_CWE",
            "genre": "DataGenreENDW_RICHNESS",
            "mimetype": "image/geotiff"
        },
        '*ENDW_SINGLE.tif': {
            "title": "Endemism whole - Unweigthed by the number of neighbours",
            "genre": "DataGenreENDW_SINGLE",
            "mimetype": "image/geotiff"
        },
        '*REDUNDANCY_SET1.tif': {
            "title": "Redundancy - neigbour set 1",
            "genre": "DataGenreREDUNDANCY_SET1",
            "mimetype": "image/geotiff",
        },
        '*REDUNDANCY_SET2.tif': {
            "title": "Redundancy - neigbour set 2",
            "genre": "DataGenreREDUNDANCY_SET2",
            "mimetype": "image/geotiff",
        },
        '*REDUNDANCY_ALL.tif': {
            "title": "Redundancy - both neigbour sets",
            "genre": "DataGenreREDUNDANCY_ALL",
            "mimetype": "image/geotiff",
        },
        '*RAREW_CWE.tif': {
            "title": "Rarity whole - Corrected weighted rarity",
            "genre": "DataGenreRAREW_CWE",
            "mimetype": "image/geotiff",
        },
        '*RAREW_RICHNESS.tif': {
            "title": "Rarity whole - Richness used in RAREW_CWE",
            "genre": "DataGenreRAREW_RICHNESS",
            "mimetype": "image/geotiff",
        },
        '*RAREW_WE.tif': {
            "title": "Rarity whole - weighted rarity",
            "genre": "DataGenreRAREW_WE",
            "mimetype": "image/geotiff",
        },
        '*.bds':  {
            # Perl Storable package
            'title': 'Biodiverse output',
            'genre': 'DataGenreBiodiverseModel',
            'mimetype': 'application/octet-stream',
        },
        '*.pl': {
            'title': 'Job Script',
            'genre': 'JobScript',
            'mimetype': 'application/x-perl',
        }
    },
    'archives': {
        # 'results.html.zip': {
        #     'files': ['results.html', 'AUC.png'],
        #     'title': 'Accuracy measures report as zip',
        #     'type': 'eval',
        #     'format': 'zip',
    },
}


@provider(IComputeMethod)
def execute(result, toolkit):
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
    ### send job to queue
    after_commit_task(perl_task, params, context)
