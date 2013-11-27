from org.bccvl.compute.utils import WorkEnv, WorkEnvLocal, queue_job
from pkg_resources import resource_string

from zope.schema.vocabulary import SimpleVocabulary
from zope.interface import moduleProvides, implementer
from z3c.form.object import registerFactoryAdapter # do this dynamically in site module?
from zope import schema
from zope.schema.fieldproperty import FieldProperty
from decimal import Decimal
from .biomod import IParametersBiomod, ParametersBiomod, get_biomod_params
from .interfaces import IComputeFunction

from org.bccvl.compute import MessageFactory as _

moduleProvides(IComputeFunction)


OUTPUTS = {
    'files': {
        '*.txt': {
            'title': '',
            'type': 'eval'},
        '*_response_curves.png': {
            'title': '',
            'type': '???'},
        '*.csv': {
            'title': '',
            'type': 'csv', },
        'model.object.RData': {
            'title': '',
            'type': 'RData', },
        'pROC.png': {
            'title': '',
            'type': '???', },
        'sdm.Rout': {
            'title': '',
            'type': 'html'},
        },
    'archives': {
        'all.zip': {
            'files': ['all/*'],
            'type': 'report'},
        },
    }


def get_cta_params(experiment):
    cta_params = experiment.parameters_cta
    params = get_biomod_params(cta_params)
    params.update({
        'method': cta_params.method,
        'control_xval': cta_params.control_xval,
        'control_minbucket': cta_params.control_minbucket,
        'control_minsplit': cta_params.control_minsplit,
        'control_cp': cta_params.control_cp,
        'control_maxdepth': cta_params.control_maxdepth,
    })
    return params


def generate_sdm_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        resource_string('org.bccvl.compute', 'rscripts/cta.R')])
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
    params.update(get_cta_params(experiment))
    script = generate_sdm_script()
    return queue_job(experiment, 'CTA', env, script, params)

## Parameters

cta_method_vocab = SimpleVocabulary.fromValues([
    'anova',
    'class',
    'exp',
    'poisson',
])


class IParametersCTA(IParametersBiomod):
    method = schema.Choice(
        title=_(u'method'),
        vocabulary=cta_method_vocab,
        default='class',
    )

    control_xval = schema.Int(
        title=_(u'cross-validations'),
        default=5,
    )
    control_minbucket = schema.Int(
        title=_(u'minimum bucket'),
        description=_(u'minimum number of observations in any terminal node'),
        default=5,
    )
    control_minsplit = schema.Int(
        title=_(u'minimum split'),
        description=_(u'minimum number of observations that must exist in a node for a split to be attempted'),
        default=5,
    )
    control_cp = schema.Decimal(
        title=_(u'complexity parameter'),
        default=Decimal('0.001'),
    )
    control_maxdepth = schema.Int(
        title=_(u'maximum depth'),
        description=_(u'maximum depth of any node of the final tree, with the root node counted as depth 0'),
        default=25,
    )

field_property = lambda field_name: FieldProperty(IParametersCTA[field_name])


@implementer(IParametersCTA)
class ParametersCTA(ParametersBiomod):
    method = field_property('method')
    control_xval = field_property('control_xval')
    control_minbucket = field_property('control_minbucket')
    control_minsplit = field_property('control_minsplit')
    control_cp = field_property('control_cp')
    control_maxdepth = field_property('control_maxdepth')

registerFactoryAdapter(IParametersCTA, ParametersCTA)

parameters = IParametersCTA

if __name__ == '__main__':
    execute()
