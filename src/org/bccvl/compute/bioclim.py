from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv, WorkEnvLocal, queue_job, getDatasetInfo, getdatasetparams

from zope.interface import moduleProvides, implementer, Interface
# do this dynamically in site module?
from z3c.form.object import registerFactoryAdapter
from .interfaces import IComputeFunction

moduleProvides(IComputeFunction)

# dsinfo:
#    environment: ... list of datasets to compute:
#      dstc: ... dsinfo + layerinfo to use


def get_sdm_params(experiment):
    # TODO: make list/single value detection possible
    #       currently all files are treated as multi select here
    # TODO: make sure param names here match field names in schema and variables in R-srript
    params = {'layers': experiment.environmental_layers,
              'occurrence': {},
              'background': {},
              'environment': {}}
    uuid = experiment.species_occurrence_dataset
    params['occurrence'][uuid] = getdatasetparams(uuid)
    uuid = experiment.species_absence_dataset
    params['background'][uuid] = getdatasetparams(uuid)
    for uuid in experiment.environmental_layers.values():
        # TODO: There might be the same uuid multiple times
        params['environment'][uuid] = getdatasetparams(uuid)
    # TODO Get rid of datasetkey (atl east out of paramete space)
    params['datasetkeys'] = ('occurrence', 'background', 'environment')
    return params


def get_bioclim_params(experiment):
    return {}


def generate_sdm_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        resource_string('org.bccvl.compute', 'rscripts/bioclim.R'),
    ])
    return script

DISMO_OUTPUTS = {
    'files': {
        'AUC.png': {
            'title': 'Area Under the Receiver Operating'
                     ' Characteristic Curve (AUC)',
            'type': 'eval',
            'format': 'png',  # TODO: replace format withe mime/type?
        },
        '*.csv': {
            'title': 'Model accuracy statistics',
            'type': 'eval',
            'format': 'csv',
        },
        'dismo.eval.object.RData': {
            'title': 'R ModelEvaluation object',
            'type': 'eval',
            'format': 'RData',
        },
        'model.object.RData': {
            'title': 'R SDM Model object',
            'type': 'model',
            'format': 'RData',
        },
        'results.html': {
            'title': 'Accuracy measures report',
            'type': 'eval',
            'format': 'html',
        },
        'sdm.Rout': {
            'title': 'Log file',
            'type': 'log',
            'format': 'txt',
        },
        'current.tif': {
            'title': 'Projection to current',
            'type': 'projection',
            'format': 'GTiff',
        },
    },
    'archives': {
        'results.html.zip': {
            'files': ['results.html', 'AUC.png'],
            'title': 'Accuracy measures report as zip',
            'type': 'eval',
            'format': 'zip',
        },
    },
}

OUTPUTS = DISMO_OUTPUTS
OUTPUTS['files'].update({
    '*_response.png': {
        'title': 'Marginal Response Curve',
        'type': 'eval',
        'format': 'png',
    },
})


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
    env = workenv('localhost', request)
    params = get_sdm_params(experiment)
    params.update( get_bioclim_params(experiment))
    script = generate_sdm_script()
    return queue_job(experiment, 'Bioclim', env, script, params, OUTPUTS)


class IParametersBioclim(Interface):

    """there are no user-configurable options"""


@implementer(IParametersBioclim)
class ParametersBioclim(object):
    pass

registerFactoryAdapter(IParametersBioclim, ParametersBioclim)

parameters = IParametersBioclim

if __name__ == '__main__':
    execute()
