import os
import os.path
from subprocess import call
from org.bccvl.compute.utils import (prepare_data, init_work_env,
                                     check_r_libs_path, store_results)
import shutil
from pkg_resources import resource_string
import glob
from plone.app.uuid.utils import uuidToObject
from jinja2 import Template


def get_datapath_for_glob(path, match):
    flist = list(glob.glob(os.path.join(path,  'enviro', match)))
    if len(flist):
        return flist[0]
    return None


def write_brt_config(rootpath, path, species):
    names = ["bioclim_01", "bioclim_04", "bioclim_05",
             "bioclim_06", "bioclim_12", "bioclim_15",
             "bioclim_16", "bioclim_17"]

    # FIXME: hardcoded date
    currentfolder = get_datapath_for_glob(path, 'current*')
    futurefolder = get_datapath_for_glob(path, '*2085')
    curdata = [os.path.join(currentfolder, name + ".tif") for name in names]
    futdata = None
    if futurefolder:
        futdata = [os.path.join(futurefolder, name + ".tif") for name in names]
    bkgdata = None
    if os.path.exists(os.path.join(path, 'species', species, 'bkgd.csv')):
        bkgdata = os.path.join(path, 'species', species, 'bkgd.csv')

    params = {
        'rlibdir': check_r_libs_path(rootpath),
        'workdir': path,
        'species': species,
        'occurence': os.path.join(path, 'species', species, 'occur.csv'),
        'background': bkgdata,
        'enviro': {
            'names': names,
            'data': curdata,
            'type': ["continuous" for i in xrange(0, len(names))],
            },
        'future': {
            'data': futdata
            }
        }

    brt_config = resource_string('org.bccvl.compute', 'rscripts/brt.init.R')
    tmpl = Template(brt_config)
    script = tmpl.render(params) + resource_string('org.bccvl.compute', 'rscripts/bioclim.R')
    scriptfile = os.path.join(path, 'brt.R')
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
    names = ["bioclim_01", "bioclim_04", "bioclim_05",
             "bioclim_06", "bioclim_12", "bioclim_15",
             "bioclim_16", "bioclim_17"]
    species = uuidToObject(experiment.species_occurrence_dataset)
    #absence = experiment.species_absence_dataset
    climate = uuidToObject(experiment.environmental_dataset)
    future = uuidToObject(experiment.climate_dataset)
    if future is None:
        future = {}
    try:
        path = init_work_env(rootpath)
        prepare_data(path, names, climate, future, species)
        script = write_brt_config(rootpath, path, species.id)
        # TODO: use script and scriptout instead of hardcoded brt.Rout etc...
        scriptout = script + "out"
        cmd = ['R', 'CMD', 'BATCH', '--no-save', '--no-restore', script, scriptout]
        ret = call(cmd, shell=False)
        # TODO: check ret for error
        # TODO: make sure script returns proper error codes
        shutil.move(os.path.join(path, 'brt.Rout'),
                    os.path.join(path, 'output_brt'))
        store_results(experiment, os.path.join(path, 'output_brt'))
    finally:
        # TODO: capture detailed error message to report to user
        if os.path.exists(path):
            shutil.rmtree(path)


if __name__ == '__main__':
    execute()
