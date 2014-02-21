from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv, queue_job, getdatasetparams
from gu.z3cform.rdf.interfaces import IGraph
from org.bccvl.site.namespace import BIOCLIM
from plone.app.uuid.utils import uuidToObject
from zope.interface import moduleProvides
# do this dynamically in site module?
from .interfaces import IComputeFunction

moduleProvides(IComputeFunction)


def get_project_params(experiment):
    params = {'sdms': {},
              'climate': {}}
    uuid = experiment.species_distribution_models
    params['sdms'][uuid] = getdatasetparams(uuid)

    sdmobj = uuidToObject(uuid)
    graph = IGraph(sdmobj)
    layers = list(graph.objects(graph.identifier, BIOCLIM['bioclimVariable']))
    params['layersperdataset'] = layers

    for brain in experiment.future_climate_datasets():
        params['climate'][brain.UID] = getdatasetparams(brain.UID)
        params['climate'][brain.UID]['archivefiles'] = [
            ]
    params['datasetkeys'] = ('sdms', 'climate')
    return params


def generate_project_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/predict.R'),
    ])
    return script

OUTPUTS = {
    'files': {
        '*.tif': {
            'title': 'Projection',
            'type': 'projection',
            'format': 'GTiff',  # TODO: replace format withe mime/type?
        },
    },
    'archives': {
        # 'results.html.zip': {
        #     'files': ['results.html', 'AUC.png'],
        #     'title': 'Accuracy measures report as zip',
        #     'type': 'eval',
        #     'format': 'zip',
    },
}


def execute(experiment, request=None, workenv=WorkEnv):
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
    env = workenv()
    params = get_project_params(experiment)
    script = generate_project_script()
    return queue_job(experiment, 'Projection', env, script, params, OUTPUTS)


parameters = None

if __name__ == '__main__':
    execute()
