import os
import os.path
from subprocess import call
from org.bccvl.compute.utils import WorkEnv
import shutil
from pkg_resources import resource_string
from plone.app.uuid.utils import uuidToObject
from jinja2 import Template


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


if __name__ == '__main__':
    execute()
