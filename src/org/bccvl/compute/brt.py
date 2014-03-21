from org.bccvl.compute.utils import WorkEnv, queue_job
from pkg_resources import resource_string

from zope.schema.vocabulary import SimpleVocabulary, SimpleTerm
from zope.schema.interfaces import IVocabularyFactory
from zope.interface import moduleProvides, implementer, Interface, provider
from persistent import Persistent
from z3c.form.object import registerFactoryAdapter # do this dynamically in site module?
from zope import schema
from zope.schema.fieldproperty import FieldProperty
from decimal import Decimal
from .interfaces import IComputeFunction
from .bioclim import get_sdm_params
from org.bccvl.compute import MessageFactory as _

moduleProvides(IComputeFunction)

from .bioclim import DISMO_OUTPUTS
OUTPUTS = DISMO_OUTPUTS


def get_brt_params(experiment):
    brt_params = experiment.parameters_brt
    params = get_sdm_params(experiment)
    params.update({
        'tree_complexity': brt_params.tree_complexity,
        'learning_rate': brt_params.learning_rate,
        'bag_fraction': brt_params.bag_fraction,
        #'var_monotone': brt_params.var_monotone,
        'n_folds': brt_params.n_folds,
        'prev_stratify': brt_params.prev_stratify,
        'family': brt_params.family,
        'n_trees': brt_params.n_trees,
        'max_trees': brt_params.max_trees,
        'tolerance_method': brt_params.tolerance_method,
        'tolerance_value': brt_params.tolerance_value
    })
    return params


def generate_sdm_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        resource_string('org.bccvl.compute', 'rscripts/brt.R')])
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
    params = get_brt_params(experiment)
    script = generate_sdm_script()
    return queue_job(experiment, 'BRT', env, script, params, OUTPUTS)


## Parameters

brt_var_monotone_vocab = SimpleVocabulary([
    SimpleTerm(-1, '-1', u'-1'),
    SimpleTerm(1, '+1', u'+1'),
])


@provider(IVocabularyFactory)
def brt_var_monotone_vocab_factory(context):
    return brt_var_monotone_vocab

brt_family_vocab = SimpleVocabulary.fromItems([
    ('bernoulli (binomial)', 'bernoulli'),
    ('poisson', 'poisson'),
    ('laplace', 'laplace'),
    ('gaussian', 'gaussian'),
])


@provider(IVocabularyFactory)
def brt_family_vocab_factory(context):
    return brt_family_vocab


brt_tolerance_method_vocab = SimpleVocabulary.fromValues([
    'auto',
    'fixed',
])


@provider(IVocabularyFactory)
def brt_tolerance_method_vocab_factory(context):
    return brt_tolerance_method_vocab


class IParametersBRT(Interface):
    tree_complexity = schema.Int(
        title=_(u'tree complexity'),
        default=1,
        description=_(u'The complexity of individual trees between 1 and 50'
                      u' (inclusive)'),
        min=1,
        max=50,
        required=False,
    )

    learning_rate = schema.Decimal(
        title=_(u'learning rate'),
        description=_(u'The weight applied to individual trees.'),
        default=Decimal('0.01'),
        required=False,
    )

    bag_fraction = schema.Decimal(
        title=_(u'bag fraction'),
        description=_(u'The proportion of observations used in selecting'
                      u' variables.'),
        default=Decimal('0.75'),
        required=False,
    )

    #form.widget(var_monotone=RadioFieldWidget) # TODO: get rid of form hints
    var_monotone = schema.Choice(
        title=_(u'var monotone'),
        default=-1,
        vocabulary='brt_var_monotone_vocab',
        required=False,
    )

    n_folds = schema.Int(
        title=_(u'n folds'),
        description=_(u'Number of folds.'),
        default=10,
        required=False,
    )

    prev_stratify = schema.Bool(
        title=_(u'prev stratify'),
        description=_(u'prevalence stratify the folds - only for'
                      u' presence/absence data'),
        default=True,
        required=False,
    )

    family = schema.Choice(
        title=_(u'family'),
        default='bernoulli',
        vocabulary='brt_family_vocab',
        required=False,
    )

    n_trees = schema.Int(
        title=_(u'trees added each cycle'),
        description=_(u'Number of initial trees to fit'),
        default=50,
        required=False,
    )

    max_trees = schema.Int(
        title=_(u'max trees'),
        description=_(u'Max number of trees to fit before stopping'),
        default=10000,
        required=False,
    )

    tolerance_method = schema.Choice(
        title=_(u'tolerance method'),
        description=_(u'Method to use in deciding to stop.'),
        default='auto',
        vocabulary='brt_tolerance_method_vocab',
        required=False,
    )

    tolerance_value = schema.Decimal(
        title=_(u'tolerance value'),
        description=_(u'Tolerance value to use - if method == fixed is'
                      u' absolute, if auto is multiplier * total mean'
                      u' deviance'),
        default=Decimal('0.001'),
        required=False,
    )


@implementer(IParametersBRT)
class ParametersBRT(Persistent):
    tree_complexity = FieldProperty(IParametersBRT['tree_complexity'])
    learning_rate = FieldProperty(IParametersBRT['learning_rate'])
    bag_fraction = FieldProperty(IParametersBRT['bag_fraction'])
    var_monotone = FieldProperty(IParametersBRT['var_monotone'])
    n_folds = FieldProperty(IParametersBRT['n_folds'])
    prev_stratify = FieldProperty(IParametersBRT['prev_stratify'])
    family = FieldProperty(IParametersBRT['family'])
    n_trees = FieldProperty(IParametersBRT['n_trees'])
    max_trees = FieldProperty(IParametersBRT['max_trees'])
    tolerance_method = FieldProperty(IParametersBRT['tolerance_method'])
    tolerance_value = FieldProperty(IParametersBRT['tolerance_value'])

registerFactoryAdapter(IParametersBRT, ParametersBRT)

parameters = IParametersBRT

if __name__ == '__main__':
    execute()
