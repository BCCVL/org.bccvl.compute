from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv, WorkEnvLocal, queue_job

from zope.schema.vocabulary import SimpleVocabulary
from zope.interface import moduleProvides, implementer, Interface
from z3c.form.object import registerFactoryAdapter # do this dynamically in site module?
from zope import schema
from zope.schema.fieldproperty import FieldProperty
from decimal import Decimal
from .interfaces import IComputeFunction
from .biomod import (IParametersBiomod,
                     ParametersBiomod,
                     get_biomod_params,
                     BIOMOD_OUTPUTS)
from org.bccvl.compute import MessageFactory as _

OUTPUTS = BIOMOD_OUTPUTS

moduleProvides(IComputeFunction)


def get_gam_params(experiment):
    gam_params = experiment.parameters_gam
    params = get_biomod_params(gam_params)
    params.update({
        'type': gam_params.type,
        'interaction_level': gam_params.interaction_level,
        'test': gam_params.test,
        'family': gam_params.family,
        'mustart': gam_params.mustart,
        'control_epsilon': gam_params.control_epsilon,
        'control_maxit': gam_params.control_maxit,
        'control_trace': gam_params.control_trace
    })
    return params


def generate_sdm_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        resource_string('org.bccvl.compute', 'rscripts/gam.R')])
    return script


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
    env = workenv('localhost', request)
    params = get_gam_params(experiment)
    script = generate_sdm_script()
    return queue_job(experiment, 'GAM', env, script, params, OUTPUTS)

## parameters

glm_type_vocab = SimpleVocabulary.fromValues([
    'simple',
    'quadratic',
    'polynomial',
])

glm_test_vocab = SimpleVocabulary.fromValues([
    'AIC',
    'BIC',
    'none',
])

glm_family_vocab = SimpleVocabulary.fromValues([
    'binomial',
    'gaussian',
    'gamma',
    'inverse.gaussian',
    'poisson',
    'quasi',
    'quasibinomial',
    'quasipoisson',
])


class IParametersGlm(IParametersBiomod):
    type = schema.Choice(
        title=_(u'type'),
        default='quadratic',
        vocabulary=glm_type_vocab,
    )

    interaction_level = schema.Int(
        title=_(u'interaction level'),
        default=0,
    )

    #myFormula = ...

    test = schema.Choice(
        title=_(u'test'),
        vocabulary=glm_test_vocab,
        default='AIC',
    )

    family = schema.Choice(
        title=_(u'family'),
        vocabulary=glm_family_vocab,
        default='binomial',
    )

    mustart = schema.Decimal(
        title=_(u'mustart'),
        description=_(u'starting values for the vector of means'),
        default=Decimal('0.5'),
    )

    control_epsilon = schema.Decimal(
        title=_(u'control: epsilon'),
        description=_(u'positive convergence tolerance e'),
        default=Decimal('1e-08'),
    )

    control_maxit = schema.Int(
        title=_(u'control: maxit'),
        description=_(u'maximal number of IWLS iterations'),
        default=50,
    )

    control_trace = schema.Bool(
        title=_(u'control: trace'),
        description=_(u'produce output for each iteration'),
        default=False,
    )


@implementer(IParametersGlm)
class ParametersGlm(ParametersBiomod):
    type = FieldProperty(IParametersGlm['type'])
    interaction_level = FieldProperty(IParametersGlm['interaction_level'])
    test = FieldProperty(IParametersGlm['test'])
    family = FieldProperty(IParametersGlm['family'])
    mustart = FieldProperty(IParametersGlm['mustart'])
    control_epsilon = FieldProperty(IParametersGlm['control_epsilon'])
    control_maxit = FieldProperty(IParametersGlm['control_maxit'])
    control_trace = FieldProperty(IParametersGlm['control_trace'])

registerFactoryAdapter(IParametersGlm, ParametersGlm)

parameters = IParametersGlm

if __name__ == '__main__':
    execute()
