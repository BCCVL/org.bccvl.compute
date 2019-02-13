from copy import deepcopy
from pkg_resources import resource_string

from plone import api
from zope.interface import provider

from org.bccvl.compute.utils import getdatasetparams
from org.bccvl.site.interfaces import IComputeMethod
from org.bccvl.site.utils import get_results_dir
from org.bccvl.tasks.plone import after_commit_task, HIGH_PRIORITY
from org.bccvl.tasks.compute import r_task


def get_ensemble_params(result):
    # make a deep copy of the params to not accedientially modify the
    # persisted dict
    params = deepcopy(result.job_params)

    # params['datasets'] is a list of dataset uuids
    # and sholud become a list of dicts with datasetinfos

    dslist = []
    for dsparam in params['datasets']:
        dslist.append(getdatasetparams(dsparam))
    params['datasets'] = dslist

    sdm_projections = []
    for uuid in params['sdm_projections']:
        sdm_projections.append(getdatasetparams(uuid))
    params['sdm_projections'] = sdm_projections

    # TODO: quick fix Decimal json encoding through celery
    params['thresholds'] = [float(val) for val in params['thresholds']]

    workerhints = {
        'files': ('datasets', 'sdm_projections',)
    }
    return {'env': {}, 'params': params, 'worker': workerhints}


def generate_ensemble_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/ensemble.R'),
    ])
    return script


OUTPUTS = {
    'files': {
        'ensemble_mean_*.tif': {
            'title': 'Summary Mean',
            'genre': 'DataGenreEnsembleResult',
            "layer": "projection_probability",
            "data_type": "Continuous",
            'mimetype': 'image/geotiff',
            "order": 1
        },
        'ensemble_min_*.tif': {
            'title': 'Summary Minimum',
            'genre': 'DataGenreEnsembleResult',
            "layer": "projection_probability",
            "data_type": "Continuous",
            'mimetype': 'image/geotiff',
            "order": 2
        },
        'ensemble_max_*.tif': {
            'title': 'Summary Maximum',
            'genre': 'DataGenreEnsembleResult',
            "layer": "projection_probability",
            "data_type": "Continuous",
            'mimetype': 'image/geotiff',
            "order": 3
        },
        'ensemble_variance_*.tif': {
            'title': 'Summary Variance',
            'genre': 'DataGenreEnsembleResult',
            "layer": "projection_probability",
            "data_type": "Continuous",
            'mimetype': 'image/geotiff',
            "order": 4
        },
        'ensemble_q0p05_*.tif': {
            'title': '5th Percentile',
            'genre': 'DataGenreEnsembleResult',
            "layer": "projection_probability",
            "data_type": "Continuous",
            'mimetype': 'image/geotiff',
            "order": 5
        },
        'ensemble_q0p1_*.tif': {
            'title': '10th Percentile',
            'genre': 'DataGenreEnsembleResult',
            "layer": "projection_probability",
            "data_type": "Continuous",
            'mimetype': 'image/geotiff',
            "order": 6
        },
        'ensemble_q0p5_*.tif': {
            'title': '50th Percentile',
            'genre': 'DataGenreEnsembleResult',
            "layer": "projection_probability",
            "data_type": "Continuous",
            'mimetype': 'image/geotiff',
            "order": 7
        },
        'ensemble_q0p9_*.tif': {
            'title': '90th Percentile',
            'genre': 'DataGenreEnsembleResult',
            "layer": "projection_probability",
            "data_type": "Continuous",
            'mimetype': 'image/geotiff',
            "order": 8
        },
        'ensemble_q0p95_*.tif': {
            'title': '95th Percentile',
            'genre': 'DataGenreEnsembleResult',
            "layer": "projection_probability",
            "data_type": "Continuous",
            'mimetype': 'image/geotiff',
            "order": 9
        },
        'ensemble_rangechange_*.tif': {
            'title': 'Change in species range map',
            'genre': 'DataGenreClimateChangeMetricMap',
            'layer': 'range_change',
            "data_type": "Discrete",
            'mimetype': 'image/geotiff',
            "order": 10
        },
        'ensemble_rangechange_*.csv': {
            'title': 'Change in species range table',
            'genre': 'DataGenreClimateChangeMetric',
            'mimetype': 'text/csv',
            "order": 11
        },
        'ensemble_meansdm_*.tif': {
            'title': 'Summary Mean',
            'genre': 'DataGenreEnsembleResult',
            "layer": "projection_probability",
            "data_type": "Continuous",
            'mimetype': 'image/geotiff',
            "order": 12
        },
        '*.R': {
            'title': 'Job Script',
            'genre': 'JobScript',
            'mimetype': 'text/x-r',
            "order": 20
        },
        '*.Rout': {
            "title": "Log file",
            "genre": "DataGenreLog",
            "mimetype": "text/x-r-transcript",
            "order": 21
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
    # FIXME: ensemble is not yet a content based toolkit
    # try:
    #     OUTPUTS = json.loads(toolkit.output)
    # except (ValueError, TypeError) as e:
    #     LOG.fatal("couldn't load OUTPUT form toolkit %s: %s",
    #               toolkit.getId(), e)
    #     OUTPUTS = {}
    params = get_ensemble_params(result)
    script = generate_ensemble_script()
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
        'name': 'ensemble.R',
        'script': script,
    }
    # set debug flag
    params['worker']['zipworkenv'] = api.env.debug_mode()
    ### send job to queue
    after_commit_task(r_task, priority, params, context)
