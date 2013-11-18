import os
import os.path
from subprocess import call
from org.bccvl.compute.utils import WorkEnv
import shutil
from pkg_resources import resource_string
from plone.app.uuid.utils import uuidToObject
from jinja2 import Template

from zope.schema.vocabulary import SimpleVocabulary
from zope.interface import moduleProvides, implementer, Interface
from persistent import Persistent
from z3c.form.object import registerFactoryAdapter # do this dynamically in site module?
from zope import schema
from zope.schema.fieldproperty import FieldProperty
from decimal import Decimal
from .biomod import IParametersBiomod, ParametersBiomod, get_biomod_params
from .interfaces import IComputeFunction

from org.bccvl.compute import MessageFactory as _

moduleProvides(IComputeFunction)

def generate_sdm_script(experiment, params):
    ann_params = experiment.parameters_ann
    get_biomod_params(ann_params, params)
    params['nbcv'] = ann_params.nbcv
    params['rang'] = ann_params.rang
    params['maxit'] = ann_params.maxit

    ann_config = resource_string('org.bccvl.compute', 'rscripts/ann.init.R')
    tmpl = Template(ann_config)
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/common.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        tmpl.render(params),
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
