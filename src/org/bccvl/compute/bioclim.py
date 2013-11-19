import os
import os.path
import shutil
from pkg_resources import resource_string
from subprocess import call
from org.bccvl.compute.utils import WorkEnv
from plone.app.uuid.utils import uuidToObject
from jinja2 import Template

from zope.interface import moduleProvides, implementer, Interface
from z3c.form.object import registerFactoryAdapter # do this dynamically in site module?
from .interfaces import IComputeFunction

moduleProvides(IComputeFunction)

def generate_sdm_script(experiment, params):
    bioclim_config = resource_string('org.bccvl.compute', 'rscripts/bioclim.init.R')
    tmpl = Template(bioclim_config)
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/common.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        tmpl.render(params),
        resource_string('org.bccvl.compute', 'rscripts/bioclim.R'),
        ])
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
    # TODO: WORKER_DIR is gone
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

class IParametersBioclim(Interface):
    """there are no user-configurable options"""

@implementer(IParametersBioclim)
class ParametersBioclim(object):
    pass

registerFactoryAdapter(IParametersBioclim, ParametersBioclim)

parameters = IParametersBioclim

if __name__ == '__main__':
    execute()
