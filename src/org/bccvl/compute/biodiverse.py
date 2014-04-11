from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv, queue_job, getdatasetparams
from zope.interface import moduleProvides
# do this dynamically in site module?
from .interfaces import IComputeFunction

moduleProvides(IComputeFunction)


def get_biodiverse_params(experiment):
    # TODO: workenv can only deal with datasets in dictinoaries, with
    # uuid as key, list would be more appropriate here as order might
    # matter
    params = {'specieslayers': {},
              'thresholds': {},
              'clustersize': experiment.cluster_size,
              # which keys describe files to transfer?
              'datasetkeys': ['specieslayers'],
              }
    for idx, proj in enumerate(experiment.projection):
        uuid = proj['dataset']
        dsparams = getdatasetparams(uuid)
        params['specieslayers'][uuid] = dsparams
        params['thresholds'][uuid] = proj['threshold']
    return params


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


def execute(result, func, request=None):
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
    experiment = result.__parent__
    env = WorkEnv()
    params = get_biodiverse_params(experiment)
    script = generate_biodiverse_script()
    return queue_job(result, 'Biodiverse', env, script, params, OUTPUTS)
