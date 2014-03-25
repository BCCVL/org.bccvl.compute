from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv, queue_job

from zope.interface import moduleProvides
from .bioclim import get_sdm_params
from .biomod import BIOMOD_OUTPUTS
from .interfaces import IComputeFunction

from org.bccvl.compute import MessageFactory as _

OUTPUTS = BIOMOD_OUTPUTS

moduleProvides(IComputeFunction)


def get_maxent_params(experiment):
    params = get_sdm_params(experiment)
    params.update(experiment.paramaters['maxent'])
    params.update({
        # TODO: some params are probably sdm specific or even
        #       per run (in case of multi runs)
        'rescale_all_models': False,  # param_object.rescale_all_models,
        'selected_models': 'all',
        'modeling_id': 'bccvl',
        'species': 'species',
        })
    return params


def generate_sdm_script(r_script):
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        r_script])
    return script


def execute(experiment, func, request=None, workenv=WorkEnv):
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
    env = workenv()
    params = get_maxent_params(experiment)
    script = generate_sdm_script(func.script)
    return queue_job(experiment, func.getId(), env, script, params, OUTPUTS)
