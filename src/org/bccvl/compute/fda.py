"""
"""
from pkg_resources import resource_string
from zope.interface import moduleProvides, implementer, Interface
from z3c.form.object import registerFactoryAdapter
from zope import schema
from zope.schema.vocabulary import SimpleVocabulary
from zope.schema.fieldproperty import FieldProperty

from org.bccvl.compute.utils import WorkEnv, queue_job
from .interfaces import IComputeFunction
from .bioclim import get_sdm_params
from .biomod import (IParametersBiomod,
                     ParametersBiomod,
                     get_biomod_params,
                     BIOMOD_OUTPUTS)
from org.bccvl.compute import MessageFactory as _

moduleProvides(IComputeFunction)

OUTPUTS = BIOMOD_OUTPUTS

def get_fda_params(experiment):
    fda_params = experiment.parameters_fda
    params = get_biomod_params(fda_params)
    params.update({
        'method': fda_params.method,
    })
    return params


def generate_fda_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        resource_string('org.bccvl.compute', 'rscripts/fda.R'),
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
    params.update(get_fda_params(experiment))
    script = generate_fda_script()
    return queue_job(experiment, 'fda', env, script, params, OUTPUTS)

## Parameters

fda_method_vocab = SimpleVocabulary.fromItems([
    ('polyreg', 'polyreg'),
    ('mars', 'mars'),
    ('bruto', 'bruto'),
    ('gen.ridge', 'gen.ridge'),
])

class IParametersFDA(IParametersBiomod):
    method = schema.Choice(
        title=_(u'method'),
        description=_(u'The regression method used in optimal scaling'),
        default='mars',
        vocabulary=fda_method_vocab,
        required=False,
    )


@implementer(IParametersFDA)
class ParametersFDA(ParametersBiomod):
    method = FieldProperty(IParametersFDA['method'])


registerFactoryAdapter(IParametersFDA, ParametersFDA)

parameters = IParametersFDA

if __name__ == '__main__':
    execute()
