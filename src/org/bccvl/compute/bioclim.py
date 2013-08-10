import os
import os.path
import shutil
from pkg_resources import resource_string
import zipfile
from subprocess import call
from org.bccvl.compute.utils import (check_r_libs_path,
                                     init_work_env,
                                     prepare_data,
                                     addFile)
import glob
from plone.app.uuid.utils import uuidToObject


BIOCLIM_CONFIG = """
.libPaths("{rlibdir}")
wd = "{workdir}"
species = "{species}"
occur.data = "{occurence}"
bkgd.data = {background}
enviro.data.names = c({enviro[names]})
enviro.data.current = c({enviro[data]})
enviro.data.type = c({enviro[type]})
enviro.data.future= c({future[data]})

model.bioclim = TRUE
project.bioclim = TRUE
model.brt = FALSE
project.brt = FALSE

opt.tails = c("both")
opt.ext = NULL
"""


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
    curdata = ",".join(('"{0}"'.format(os.path.join(currentfolder, name + ".tif")) for name in names))
    futdata = ''
    if futurefolder:
        futdata = ",".join(('"{0}"'.format(os.path.join(futurefolder, name + ".tif")) for name in names))

    params = {
        'rlibdir': check_r_libs_path(rootpath),
        'workdir': path,
        'species': species,
        'occurence': os.path.join(path, 'species', species, 'occur.csv'),
        'background': "NULL",  # os.path.join(path, 'species', species, 'background.csv')
        'enviro': {
            'names': ",".join(('"{0}"'.format(name) for name in names)),
            'data': curdata,
            'type': ",".join(('"continuous"' for i in xrange(0, len(names))))
            },
        'future': {
            'data': futdata
            }
        }

    script = BIOCLIM_CONFIG.format(**params) + resource_string('org.bccvl.compute', 'rscripts/bioclim.R')
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
    species = uuidToObject(experiment.species_occurrence_dataset)
    climate = uuidToObject(experiment.environmental_dataset)
    try:
        path = init_work_env(rootpath)
        prepare_data(path, names, climate, species)
        script = write_bioclim_config(rootpath, path, species.id)
        scriptout = script + "out"
        cmd = ['R', 'CMD', 'BATCH', '--no-save', '--no-restore', script, scriptout]
        ret = call(cmd, shell=False)
        # TODO: check ret for error
        # TODO: make sure script returns proper error codes
        # TODO: zip result and store on experiment
        with zipfile.ZipFile(os.path.join(path, 'output.zip'), 'w', zipfile.ZIP_DEFLATED) as zipf:
            for fname in os.listdir(os.path.join(path, 'output_bioclim')):
                zipf.write(os.path.join(path, 'output_bioclim', fname), fname)
            zipf.write(os.path.join(path, 'bioclim.Rout'), 'bioclim.Rout')
        addFile(experiment,
                # TODO: IStorage adapter necessary
                #filename=u'file://' + os.path.join(path, 'output.zip'),
                filename=os.path.join(path, 'output.zip'),
                mimetype='application/zip')
    finally:
        # TODO: maybe we should try to capture Rout etc, and send it back as status message
        if os.path.exists(path):
            shutil.rmtree(path)


if __name__ == '__main__':
    execute()
