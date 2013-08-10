import os
import os.path
import zipfile
from org.bccvl.compute.utils import (prepare_data, init_work_env, addFile,
                                     check_r_libs_path)
import shutil
from pkg_resources import resource_string
import glob
from plone.app.uuid.utils import uuidToObject


BRT_CONFIG = """
.libPaths("{rlibdir}")
wd = "{workdir}"
species = "{species}"
occur.data = "{occurence}"
bkgd.data = "{background}"
enviro.data.names = c({enviro[names]})
enviro.data.current = c({enviro[data]})
enviro.data.type = c({enviro[type]})
enviro.data.future= c({future[data]})

model.bioclim = FALSE
project.bioglim = FALSE
model.brt = TRUE #boolean to run Boosted regression tree algorithm
project.brt = TRUE #boolean to project Boosted regression tree algorithm

brt.fold.vector = NULL #a fold vector to be read in for cross validation with offsets
brt.tree.complexity = 1 #sets the complexity of individual trees
brt.learning.rate = 0.01 #sets the weight applied to individual trees
brt.bag.fraction = 0.75 #sets the proportion of observations used in selecting variables
#brt.site.weights = rep(1, nrow(data)) #allows varying weighting for sites
#brt.var.monotone = rep(0, length(gbm.x)) #restricts responses to individual predictors to monotone
brt.n.folds = 10 #number of folds
brt.prev.stratify = TRUE #prevalence stratify the folds - only for presence/absence data
brt.family = "bernoulli" #family - bernoulli (=binomial), poisson, laplace or gaussian
brt.n.trees = 50 #number of initial trees to fit
brt.step.size = brt.n.trees #numbers of trees to add at each cycle
brt.max.trees = 10000 #max number of trees to fit before stopping
brt.tolerance.method = "auto" #method to use in deciding to stop - "fixed" or "auto"
brt.tolerance = 0.001 #tolerance value to use - if method == fixed is absolute, if auto is multiplier * total mean deviance
brt.keep.data = FALSE #Logical. keep raw data in final model
brt.plot.main = FALSE #Logical. plot hold-out deviance curve
brt.plot.folds = FALSE #Logical. plot the individual folds as well
brt.verbose = FALSE #Logical. control amount of screen reporting
brt.silent = FALSE #Logical. to allow running with no output for simplifying model)
brt.keep.fold.models = FALSE #Logical. keep the fold models from cross valiation
brt.keep.fold.vector = FALSE #Logical. allows the vector defining fold membership to be kept
brt.keep.fold.fit = FALSE #Logical. allows the predicted values for observations from cross-validation to be kept


"""


def get_datapath_for_glob(path, match):
    flist = list(glob.glob(os.path.join(path,  'enviro', match)))
    if len(flist):
        return flist[0]
    return None


def write_brt_config(rootpath, path, species):
    names = ["bioclim_01", "bioclim_04", "bioclim_05",
             "bioclim_06", "bioclim_12", "bioclim_15",
             "bioclim_16", "bioclim_17"]

    currentfolder = get_datapath_for_glob(path, 'current*')
    futurefolder = get_datapath_for_glob(path, '*2085')
    curdata = ",".join(('"{0}"'.format(os.path.join(currentfolder, name + ".tif")) for name in names))
    futdata = ''
    if futurefolder:
        futdata = ",".join(('"{0}"'.format(os.path.join(futurefolder, name + ".tif")) for name in names))

    params = {
        'rlibdir': check_r_libs_path(rootpath),
        'workdir': path,
        'species': species,
        'occurence': os.path.join(path, 'species', species, 'occur.csv'),
        'background': os.path.join(path, 'species', species, 'bkgd.csv'),
        'enviro': {
            'names': ",".join(('"{0}"'.format(name) for name in names)),
            'data': curdata,
            'type': ",".join(('"continuous"' for i in xrange(0, len(names))))
            },
        'future': {
            'data': futdata
            }
        }

    script = BRT_CONFIG.format(**params) + resource_string('org.bccvl.compute', 'rscripts/brt.R')
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
    try:
        path = init_work_env(rootpath, species.id)
        prepare_data(path, names, climate, species)
        script = write_brt_config(rootpath, path, species.id)
        # TODO: use script and scriptout instead of hardcoded brt.Rout etc...
        scriptout = script + "out"
        cmd = ['R', 'CMD', 'BATCH', '--no-save', '--no-restore', script, scriptout]
        ret = call(cmd, shell=False)
        # TODO: check ret for error
        # TODO: make sure script returns proper error codes
        # TODO: zip result and store on experiment
        with zipfile.ZipFile(os.path.join(path, 'output.zip'), 'w', zipfile.ZIP_DEFLATED) as zipf:
            for fname in os.listdir(os.path.join(path, 'output_brt')):
                zipf.write(os.path.join(path, 'output_brt', fname), fname)
            zipf.write(os.path.join(path, 'brt.Rout'), 'brt.Rout')
        addFile(experiment,
                # TODO: need IStorage adapter for urllib.urlinfo
                #filename=u'file://' + os.path.join(path, 'output.zip'),
                filename=os.path.join(path, 'output.zip'),
                mimetype='application/zip')
    finally:
        # TODO: capture detailed error message to report to user
        if os.path.exists(path):
            shutil.rmtree(path)


if __name__ == '__main__':
    execute()
