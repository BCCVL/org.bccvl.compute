import os
import os.path
import shutil
from pkg_resources import resource_string
from subprocess import call
from org.bccvl.compute.utils import (prepare_data,
                                     init_work_env,
                                     store_results,
                                     get_sdm_params)
from plone.app.uuid.utils import uuidToObject
from jinja2 import Template


def write_bioclim_config(rootpath, path, species):
    params = get_sdm_params(rootpath, path, species)
    bioclim_config = resource_string('org.bccvl.compute', 'rscripts/bioclim.init.R')
    tmpl = Template(bioclim_config)
    script = tmpl.render(params) + resource_string('org.bccvl.compute', 'rscripts/bioclim.R')
    scriptfile = os.path.join(path, 'bioclim.R')
    f = open(scriptfile, "w")
    f.write(script)
    f.close()
    return scriptfile


def execute(experiment):
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
    rootpath = os.environ.get('WORKER_DIR') or os.enivron['HOME']
    occurrence = uuidToObject(experiment.species_occurrence_dataset)
    absence = uuidToObject(experiment.species_absence_dataset)
    climate = uuidToObject(experiment.environmental_dataset)
    future = uuidToObject(experiment.climate_dataset)
    result = None
    rout = None
    if future is None:
        future = {}
    try:
        path = init_work_env(rootpath)
        result = os.path.join(path, 'output_bioclim')
        rout = os.path.join(path, 'bioclim.Rout')
        prepare_data(path, climate, future, occurrence, absence)
        # FIXME: assumes, occurrence and absence use the same id
        # FIXME: species id should come from somewhere else
        script = write_bioclim_config(rootpath, path, occurrence.__parent__.id)
        # TODO: use script and scriptout instead of hardcoded brt.Rout etc...
        scriptout = script + "out"
        cmd = ['R', 'CMD', 'BATCH', '--no-save', '--no-restore', script, scriptout]
        ret = call(cmd, shell=False)
        # TODO: check ret for error
        # TODO: make sure script returns proper error codes
    except Exception, e:
        # This is really bad we have to do something with the exception
        pass
    # let's try and capture some results
    try:
        # the script went fine or not. Grab whatever is left and store it as result
        if result is None:
            # we couldn't even create the work env
            return
        if not os.path.exists(result):
            # something went wrong with the script, try to capture at least .Rout
            os.mkdir(result)
        if os.path.exists(rout):
            # move rout to result folder
            shutil.move(rout, result)
            store_results(experiment, result)
    finally:
        # hopefully all went well and we can remove our work folder
        if os.path.exists(path):
            shutil.rmtree(path)


if __name__ == '__main__':
    execute()
