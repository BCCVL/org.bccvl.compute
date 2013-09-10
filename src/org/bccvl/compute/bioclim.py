import os
import os.path
import shutil
from pkg_resources import resource_string
from subprocess import call
from org.bccvl.compute.utils import (check_r_libs_path,
                                     init_work_env,
                                     prepare_data,
                                     store_results)
import glob
from plone.app.uuid.utils import uuidToObject
from jinja2 import Template


def get_datapath_for_glob(path, match):
    flist = list(glob.glob(os.path.join(path,  'enviro', match)))
    if len(flist):
        return flist[0]
    return None


def write_bioclim_config(rootpath, path, species):
    names = ["bioclim_01", "bioclim_04", "bioclim_05",
             "bioclim_06", "bioclim_12", "bioclim_15",
             "bioclim_16", "bioclim_17"]

    currentfolder = get_datapath_for_glob(path, 'current*')
    futurefolder = get_datapath_for_glob(path, '*2085')
    # maybe use glob here?
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
    names = ["bioclim_01", "bioclim_04", "bioclim_05",
             "bioclim_06", "bioclim_12", "bioclim_15",
             "bioclim_16", "bioclim_17"]
    occurence = uuidToObject(experiment.species_occurrence_dataset)
    absence = uuidToObject(experiment.species_absence_dataset)
    climate = uuidToObject(experiment.environmental_dataset)
    future = uuidToObject(experiment.climate_dataset)
    if future is None:
        future = {}
    try:
        path = init_work_env(rootpath)
        prepare_data(path, names, climate, future, occurence, absence)
        # FIXME: assumes, occurence and absence use the same id
        script = write_bioclim_config(rootpath, path, occurence.id)
        scriptout = script + "out"
        cmd = ['R', 'CMD', 'BATCH', '--no-save', '--no-restore', script, scriptout]
        ret = call(cmd, shell=False)
        # TODO: check ret for error
        # TODO: make sure script returns proper error codes
        shutil.move(os.path.join(path, 'bioclim.Rout'),
                    os.path.join(path, 'output_bioclim'))
        store_results(experiment, os.path.join(path, 'output_bioclim'))
    finally:
        # TODO: maybe we should try to capture Rout etc, and send it back as status message
        if os.path.exists(path):
            shutil.rmtree(path)


if __name__ == '__main__':
    execute()
