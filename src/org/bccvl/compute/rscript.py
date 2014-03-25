from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv, queue_job, getdatasetparams


def get_sdm_params(experiment):
    # TODO: make list/single value detection possible
    #       currently all files are treated as multi select here
    # TODO: make sure param names here match field names in schema and
    #       variables in R-srript
    params = {
        'layers': experiment.environmental_datasets,
        'occurrence': {},
        'background': {},
        'environment': {},
        'pseudoabsences': {
            'enabled': experiment.species_pseudo_absence_points,
            'points': experiment.species_number_pseudo_absence_points
            }
        }
    uuid = experiment.species_occurrence_dataset
    params['occurrence'][uuid] = getdatasetparams(uuid)
    # TODO: background might be none?
    uuid = experiment.species_absence_dataset
    params['background'][uuid] = getdatasetparams(uuid)
    for uuid in experiment.environmental_datasets.keys():
        # TODO: There might be the same uuid multiple times
        params['environment'][uuid] = getdatasetparams(uuid)
    # TODO Get rid of datasetkey (atl east out of paramete space)
    params['datasetkeys'] = ('occurrence', 'background', 'environment')
    return params


def get_toolkit_params(experiment, toolkit):
    params = get_sdm_params(experiment)
    params.update(experiment.paramaters[toolkit.getId()])
    params.update({
        # TODO: some params are probably sdm specific or even
        #       per run (in case of multi runs)
        #       following are mostly biomod specific but could be generic
        'rescale_all_models': False,  # param_object.rescale_all_models,
        'selected_models': 'all',
        'modeling_id': 'bccvl',
        'species': 'species',
        })
    return params


def generate_sdm_script(r_script):
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        r_script])
    return script


# TODO: remove OUTPUTS
def execute_sdm(experiment, toolkit, request=None, workenv=WorkEnv,
                OUTPUTS=None):
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
    params = get_toolkit_params(experiment, toolkit)
    script = generate_sdm_script(toolkit.script)
    return queue_job(experiment, toolkit.getId(), env, script, params, OUTPUTS)
