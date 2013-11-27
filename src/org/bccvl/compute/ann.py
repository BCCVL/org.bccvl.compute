from org.bccvl.compute.utils import WorkEnv, WorkEnvLocal, queue_job
from pkg_resources import resource_string

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


def get_ann_params(experiment):
    ann_params = experiment.parameters_ann
    params = get_biomod_params(ann_params)
    params.update({
        'nbcv': ann_params.nbcv,
        'rang': ann_params.rang,
        'maxit': ann_params.maxit,
    })
    return params


def generate_sdm_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        resource_string('org.bccvl.compute', 'rscripts/ann.R')])
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
    params.update(get_ann_params(experiment))
    script = generate_sdm_script()
    return queue_job(experiment, 'ANN', env, script, params)

## Parameters

class IParametersANN(IParametersBiomod):
    nbcv = schema.Int(
        title=_(u'NbCV'),
        description=_(u'nb of cross validation to find best size and decay parameters'),
        default=5,
        required=False,
    )

    rang = schema.Decimal(
        title=_(u'rang'),
        description=_(u'Initial random weights'),
        default=Decimal('0.1'),
        required=False,
    )

    maxit = schema.Int(
        title=_(u'maxit'),
        description=_(u'Maximum number of iterations'),
        default=100,
        required=False,
    )

field_property = lambda field_name: FieldProperty(IParametersANN[field_name])


@implementer(IParametersANN)
class ParametersANN(ParametersBiomod):
    nbcv = field_property('nbcv')
    rang = field_property('rang')
    maxit = field_property('maxit')

registerFactoryAdapter(IParametersANN, ParametersANN)

parameters = IParametersANN

if __name__ == '__main__':
    execute()
