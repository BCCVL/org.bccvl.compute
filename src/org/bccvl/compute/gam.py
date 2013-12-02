from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv, WorkEnvLocal, queue_job

from zope.schema.vocabulary import SimpleVocabulary
from zope.interface import moduleProvides, implementer, Interface
from z3c.form.object import registerFactoryAdapter # do this dynamically in site module?
from zope import schema
from zope.schema.fieldproperty import FieldProperty
from decimal import Decimal
from .interfaces import IComputeFunction
from .biomod import get_biomod_params
from org.bccvl.compute import MessageFactory as _

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


def execute(experiment, workenv=WorkEnv):
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
    workenv = WorkEnvLocal
    env = workenv('localhost')
    params = env.get_sdm_params()
    params.update(get_gam_params(experiment))
    script = generate_sdm_script()
    return queue_job(experiment, 'GAM', env, script, params)

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


class IParametersGlm(Interface):
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

field_property = lambda field_name: FieldProperty(IParametersGlm[field_name])


@implementer(IParametersGlm)
class ParametersGlm(object):
    type = field_property('type')
    interaction_level = field_property('interaction_level')
    test = field_property('test')
    family = field_property('family')
    mustart = field_property('mustart')
    control_epsilon = field_property('control_epsilon')
    control_maxit = field_property('control_maxit')
    control_trace = field_property('control_trace')

registerFactoryAdapter(IParametersGlm, ParametersGlm)

parameters = IParametersGlm

if __name__ == '__main__':
    execute()