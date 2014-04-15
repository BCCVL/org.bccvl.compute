from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv, queue_job, getdatasetparams
# do this dynamically in site module?
from zope.interface import provider
from org.bccvl.site.interfaces import IComputeMethod
from copy import deepcopy


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
def execute(result, func):
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
    # TODO: CREATE WorkEnv in job
    # workenv = WorkEnvLocal
    env = WorkEnv()
    params = get_biodiverse_params(result)
    script = generate_biodiverse_script()
    return queue_job(result, 'Biodiverse', env, script, params, OUTPUTS)
