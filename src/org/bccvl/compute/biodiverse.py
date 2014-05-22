from pkg_resources import resource_string
from org.bccvl.compute.utils import getdatasetparams
# do this dynamically in site module?
from zope.interface import provider
from org.bccvl.site.interfaces import IComputeMethod
from plone import api
from copy import deepcopy
from org.bccvl.tasks.compute import biodiverse_task
from org.bccvl.tasks.plone import after_commit_task
import json
import logging
import tempfile

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
        '*.tif': {
            'title': 'Biodiverse output',
            'genre': 'BiodiverseOutput',
            'mimetype': 'image/geotiff',
        },
        # TODO: is this a Cadcorp SIS Base Dataset? (a gis file)
        '*.bds':  {
            'title': 'Biodiverse output',
            'genre': 'BiodiverseOutput',
            'mimetype': 'application/octet-stream',
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
    try:
        OUTPUTS = json.loads(toolkit.output)
    except (ValueError, TypeError) as e:
        LOG.fatal("couldn't load OUTPUT form toolkit %s: %s",
                  toolkit.getId(), e)
        OUTPUTS = {}
    params = get_biodiverse_params(result)
    script = generate_biodiverse_script()
    context = {
        'context': '/'.join(result.getPhysicalPath()),
        'userid': api.get_current().getId()
    }
    params['result'] = {
        'results_dir': tempfile.mkdtemp(),
        'outputs': OUTPUTS
    }
    params['worker']['script'] = {
        'name': 'biodiverse.pl',
        'script': script,
    }
    ### send job to queue
    after_commit_task(biodiverse_task, params, context)
