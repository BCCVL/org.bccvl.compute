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

from .bioclim import get_sdm_params

def get_domain_params(experiment):
    return {}

def generate_domain_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        resource_string('org.bccvl.compute', 'rscripts/domain.R'),
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
    params.update(get_domain_params(experiment))
    script = generate_domain_script()
    return queue_job(experiment, 'domain', env, script, params, OUTPUTS)


class IParametersDomain(Interface):
    """there are no user-configurable options"""


@implementer(IParametersDomain)
class ParametersDomain(object):
    pass


registerFactoryAdapter(IParametersDomain, ParametersDomain)

parameters = IParametersDomain

if __name__ == '__main__':
    execute()
