from pkg_resources import resource_string
from zope.interface import provider
from org.bccvl.site.interfaces import IComputeMethod
from copy import deepcopy
from org.bccvl.compute.utils import getdatasetparams
from plone import api
import tempfile
from org.bccvl.tasks.plone import after_commit_task
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
    # replace datasets param
    params['datasets'] = dslist

    workerhints = {
        'files': ('datasets', )
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
        '*.Rout': {
            "title": "Log file",
            "genre": "DataGenreLog",
            "mimetype": "text/x-r-transcript"
        },
        '*_mean.tif': {
            'title': 'Summary Mean',
            'genre': 'DataGenreEnsembleResult',
            'mimetype': 'image/geotiff',
        },
        '*_max.tif': {
            'title': 'Summary Maximum',
            'genre': 'DataGenreEnsembleResult',
            'mimetype': 'image/geotiff',
        },
        '*_min.tif': {
            'title': 'Summary Minimum',
            'genre': 'DataGenreEnsembleResult',
            'mimetype': 'image/geotiff',
        },
        '*_variance.tif': {
            'title': 'Summary Variance',
            'genre': 'DataGenreEnsembleResult',
            'mimetype': 'image/geotiff',
        },
        '*_q0p05.tif': {
            'title': '5th Percentile',
            'genre': 'DataGenreEnsembleResult',
            'mimetype': 'image/geotiff',
        },
        '*_q0p1.tif': {
            'title': '10th Percentile',
            'genre': 'DataGenreEnsembleResult',
            'mimetype': 'image/geotiff',
        },
        '*_q0p5.tif': {
            'title': '50th Percentile',
            'genre': 'DataGenreEnsembleResult',
            'mimetype': 'image/geotiff',
        },
        '*_q0p9.tif': {
            'title': '90th Percentile',
            'genre': 'DataGenreEnsembleResult',
            'mimetype': 'image/geotiff',
        },
        '*_q0p95.tif': {
            'title': '95th Percentile',
            'genre': 'DataGenreEnsembleResult',
            'mimetype': 'image/geotiff',
        },
        '*.R': {
            'title': 'Job Script',
            'genre': 'JobScript',
            'mimetype': 'text/x-r',
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
        'results_dir': 'scp://plone@127.0.0.1' + tempfile.mkdtemp(),
        'outputs': OUTPUTS
    }
    params['worker']['script'] = {
        'name': 'ensemble.R',
        'script': script,
    }
    # set debug flag
    params['worker']['zipworkenv'] = api.env.debug_mode()
    ### send job to queue
    after_commit_task(r_task, params, context)
