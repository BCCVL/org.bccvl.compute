"""
"""
from pkg_resources import resource_string
from zope.interface import moduleProvides, implementer, Interface
from z3c.form.object import registerFactoryAdapter

from org.bccvl.compute.utils import WorkEnv, queue_job
from .interfaces import IComputeFunction

moduleProvides(IComputeFunction)

from .bioclim import DISMO_OUTPUTS
OUTPUTS = DISMO_OUTPUTS

def get_sdm_params(experiment):
    # TODO: make list/single value detection possible
    #       currently all files are treated as multi select here
    # TODO: make sure param names here match field names in schema and
    #       variables in R-srript
    params = {'layers': experiment.environmental_datasets,
              'occurrence': {},
              'background': {},
              'environment': {}}
    uuid = experiment.species_occurrence_dataset
    params['occurrence'][uuid] = getdatasetparams(uuid)
    uuid = experiment.species_absence_dataset
    params['background'][uuid] = getdatasetparams(uuid)
    for uuid in experiment.environmental_datasets.keys():
        # TODO: There might be the same uuid multiple times
        params['environment'][uuid] = getdatasetparams(uuid)
    # TODO Get rid of datasetkey (atl east out of paramete space)
    params['datasetkeys'] = ('occurrence', 'background', 'environment')
    return params

def get_circles_params(experiment):
    return {}

def generate_circles_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        resource_string('org.bccvl.compute', 'rscripts/circles.R'),
    ])
    return script


def execute(experiment, request=None, workenv=WorkEnv):
    """
    This function takes an experiment and executes.

    It uses envirnoment variables WORKER_DIR or HOME as root folder to execute
    experiments.

    After the execution finishes the output files will be attached to the
    experiment.

    :param experiment: The experiment holding the configuration and receiving
                       the results
    :type experiment: org.bccvl.site.content.IExperiment


    """
    env = workenv()
    params = get_sdm_params(experiment)
    params.update(get_circles_params(experiment))
    script = generate_circles_script()
    return queue_job(experiment, 'Circles', env, script, params, OUTPUTS)


class IParametersCircles(Interface):
    """there are no user-configurable options"""


@implementer(IParametersCircles)
class ParametersCircles(object):
    pass


registerFactoryAdapter(IParametersCircles, ParametersCircles)

parameters = IParametersCircles


if __name__ == '__main__':
    execute()
