import os
import os.path
from subprocess import call
from org.bccvl.compute.utils import WorkEnv
import shutil
from pkg_resources import resource_string
from plone.app.uuid.utils import uuidToObject
from jinja2 import Template

from zope.schema.vocabulary import SimpleVocabulary, SimpleTerm
from zope.interface import moduleProvides, implementer, Interface
from persistent import Persistent
from z3c.form.object import registerFactoryAdapter # do this dynamically in site module?
from zope import schema
from zope.schema.fieldproperty import FieldProperty
from decimal import Decimal
from .interfaces import IComputeFunction

from org.bccvl.compute import MessageFactory as _

moduleProvides(IComputeFunction)

def generate_sdm_script(experiment, params):
    brt_params = experiment.parameters_brt
    params['tree_complexity'] = brt_params.tree_complexity
    params['learning_rate'] = brt_params.learning_rate
    params['bag_fraction'] = brt_params.bag_fraction
    #params['var_monotone'] = brt_params.var_monotone
    params['n_folds'] = brt_params.n_folds
    params['prev_stratify'] = brt_params.prev_stratify and "TRUE" or "FALSE"
    params['family'] = brt_params.family
    params['n_trees'] = brt_params.n_trees
    params['max_trees'] = brt_params.max_trees
    params['tolerance_method'] = brt_params.tolerance_method
    params['tolerance_value'] = brt_params.tolerance_value

    brt_config = resource_string('org.bccvl.compute', 'rscripts/brt.init.R')
    tmpl = Template(brt_config)
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/common.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        tmpl.render(params),
        resource_string('org.bccvl.compute', 'rscripts/brt.R')])
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
    occurrence = uuidToObject(experiment.species_occurrence_dataset)
    absence = uuidToObject(experiment.species_absence_dataset)
    climate = uuidToObject(experiment.environmental_dataset)
    try:
        from org.bccvl.compute.utils import WorkEnvLocal
        workenv = WorkEnvLocal
        env = workenv('localhost')
        env.prepare_work_env(climate, occurrence, absence)
        params = env.get_sdm_params()
        script = generate_sdm_script(experiment, params)
        env.execute(script)
        env.import_output(experiment)
    finally:
        env.cleanup()


## Parameters

brt_var_monotone_vocab = SimpleVocabulary([
    SimpleTerm(-1, '-1', u'-1'),
    SimpleTerm(1, '+1', u'+1'),
])

brt_family_vocab = SimpleVocabulary.fromItems([
    ('bernoulli (binomial)', 'bernoulli'),
    ('poisson', 'poisson'),
    ('laplace', 'laplace'),
    ('gaussian', 'gaussian'),
])

brt_tolerance_method_vocab = SimpleVocabulary.fromValues([
    'auto',
    'fixed',
])


class IParametersBRT(Interface):
    tree_complexity = schema.Int(
        title=_(u'tree complexity'),
        default=1,
        description=_(u'The complexity of individual trees between 1 and 50 (inclusive)'),
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
        description=_(u'The proportion of observations used in selecting variables.'),
        default=Decimal('0.75'),
        required=False,
    )

    #form.widget(var_monotone=RadioFieldWidget) # TODO: get rid of form hints
    var_monotone = schema.Choice(
        title=_(u'var monotone'),
        default=-1,
        vocabulary=brt_var_monotone_vocab,
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
        description=_(u'prevalence stratify the folds - only for presence/absence data'),
        default=True,
        required=False,
    )

    family = schema.Choice(
        title=_(u'family'),
        default='bernoulli',
        vocabulary=brt_family_vocab,
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
        default=1000,
        required=False,
    )

    tolerance_method = schema.Choice(
        title=_(u'tolerance method'),
        description=_(u'Method to use in deciding to stop.'),
        default='auto',
        vocabulary=brt_tolerance_method_vocab,
        required=False,
    )

    tolerance_value = schema.Decimal(
        title=_(u'tolerance value'),
        description=_(u'Tolerance value to use - if method == fixed is absolute, if auto is multiplier * total mean deviance'),
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
