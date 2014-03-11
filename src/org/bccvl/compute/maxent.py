from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv, queue_job

from zope.schema.vocabulary import SimpleVocabulary
from zope.interface import moduleProvides, implementer
from z3c.form.object import registerFactoryAdapter # do this dynamically in site module?
from zope import schema
from zope.schema.fieldproperty import FieldProperty
from decimal import Decimal
from .bioclim import get_sdm_params
from .biomod import (IParametersBiomod,
                     ParametersBiomod,
                     get_biomod_params,
                     BIOMOD_OUTPUTS)
from .interfaces import IComputeFunction

from org.bccvl.compute import MessageFactory as _

OUTPUTS = BIOMOD_OUTPUTS

moduleProvides(IComputeFunction)


def get_maxent_params(experiment):
    maxent_params = experiment.parameters_maxent
    params = get_sdm_params(experiment)
    params.update(get_biomod_params(maxent_params))
    params.update({
        #'path_to_maxent_jar': maxent_params.path_to_maxent_jar,
        #'memory_allocated': maxent_params.memory_allocated,
        'maximumiterations': maxent_params.maximumiterations,
        'visible': False,
        'linear': maxent_params.linear,
        'quadratic': maxent_params.quadratic,
        'product': maxent_params.product,
        'threshold': maxent_params.threshold,
        'hinge': maxent_params.hinge,
        'lq2lqptthreshold': maxent_params.lq2lqptthreshold,
        'l2lqthreshold': maxent_params.l2lqthreshold,
        'hingethreshold': maxent_params.hingethreshold,
        'beta_threshold': maxent_params.beta_threshold,
        'beta_categorical': maxent_params.beta_categorical,
        'beta_lqp': maxent_params.beta_lqp,
        'beta_hinge': maxent_params.beta_hinge,
        'defaultprevalence': maxent_params.defaultprevalence
    })
    return params


def generate_sdm_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        resource_string('org.bccvl.compute', 'rscripts/maxent.R')])
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
    env = workenv()
    params = get_maxent_params(experiment)
    script = generate_sdm_script()
    return queue_job(experiment, 'MAXENT', env, script, params, OUTPUTS)


class IParametersMaxent(IParametersBiomod):

    maximumiterations = schema.Int(
        title=_(u'maximumiterations'),
        description=_(u'maximum iterations done'),
        default=200,
    )

    linear = schema.Bool(
        title=_(u'linear'),
        description=_(u'allow linear features to be used'),
        default=True,
        required=False,
    )

    quadratic = schema.Bool(
        title=_(u'quadratic'),
        description=_(u'allow quadratic features to be used'),
        default=True,
        required=False,
    )

    product = schema.Bool(
        title=_(u'product'),
        description=_(u'allow product features to be used'),
        default=True,
        required=False,
    )

    threshold = schema.Bool(
        title=_(u'threshold'),
        description=_(u'allow threshold features to be used'),
        default=True,
        required=False,
    )

    hinge = schema.Bool(
        title=_(u'hinge'),
        description=_(u'allow hinge features to be used'),
        default=True,
        required=False,
    )

    lq2lqptthreshold = schema.Int(
        title=_(u'lq2lqptthreshold'),
        description=_(u'number of samples at which product and threshold features start being used'),
        default=80,
    )

    l2lqthreshold = schema.Int(
        title=_(u'l2lqthreshold'),
        description=_(u'number of samples at which quadratic features start being used'),
        default=10,
    )

    hingethreshold = schema.Int(
        title=_(u'hingethreshold'),
        description=_(u'number of samples at which hinge features start being used'),
        default=15,
    )

    beta_threshold = schema.Decimal(
        title=_(u'beta_threshold'),
        description=_(u'regularization parameter to be applied to all threshold features; negative value enables automatic setting'),
        default=Decimal('-1.0'),
    )

    beta_categorical = schema.Decimal(
        title=_(u'beta_categorical'),
        description=_(u'regularization parameter to be applied to all categorical features; negative value enables automatic setting'),
        default=Decimal('-1.0'),
    )

    beta_lqp = schema.Decimal(
        title=_(u'beta_lqp'),
        description=_(u'regularization parameter to be applied to all linear, quadratic and product features; negative value enables automatic setting'),
        default=Decimal('-1.0'),
    )

    beta_hinge = schema.Decimal(
        title=_(u'beta_hinge'),
        description=_(u'regularization parameter to be applied to all hinge features; negative value enables automatic setting'),
        default=Decimal('-1.0'),
    )

    defaultprevalence = schema.Decimal(
        title=_(u'defaultprevalence'),
        description=_(u'default prevalence of the species: probability of presence at ordinary occurrence points'),
        default=Decimal('0.5'),
    )


@implementer(IParametersMaxent)
class ParametersMaxent(ParametersBiomod):
    maximumiterations = FieldProperty(IParametersMaxent['maximumiterations'])
    linear = FieldProperty(IParametersMaxent['linear'])
    quadratic = FieldProperty(IParametersMaxent['quadratic'])
    product = FieldProperty(IParametersMaxent['product'])
    threshold = FieldProperty(IParametersMaxent['threshold'])
    hinge = FieldProperty(IParametersMaxent['hinge'])
    lq2lqptthreshold = FieldProperty(IParametersMaxent['lq2lqptthreshold'])
    l2lqthreshold = FieldProperty(IParametersMaxent['l2lqthreshold'])
    hingethreshold = FieldProperty(IParametersMaxent['hingethreshold'])
    beta_threshold = FieldProperty(IParametersMaxent['beta_threshold'])
    beta_categorical = FieldProperty(IParametersMaxent['beta_categorical'])
    beta_lqp = FieldProperty(IParametersMaxent['beta_lqp'])
    beta_hinge = FieldProperty(IParametersMaxent['beta_hinge'])
    defaultprevalence = FieldProperty(IParametersMaxent['defaultprevalence'])

registerFactoryAdapter(IParametersMaxent, ParametersMaxent)

parameters = IParametersMaxent

if __name__ == '__main__':
    execute()
