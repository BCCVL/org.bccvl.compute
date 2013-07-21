from subprocess import check_output, CalledProcessError, call
import os
import os.path
import shutil
from tempfile import mkdtemp
from pkg_resources import resource_string
import org.bccvl.compute
import zipfile

BIOCLIM_CONFIG="""
wd = "{workdir}"
species = "{species}"
occur.data = "{occurence}"
bkgd.data = {background}
enviro.data.names = c({enviro[names]})
enviro.data = c({enviro[data]})
enviro.data.type = c({enviro[type]})

model.bioclim = TRUE
opt.tails = c("both")
project.bioclim = TRUE
opt.ext = NULL
"""


def get_r_libs_user():
    cmd = ['R', '-e', 'write(noquote(Sys.getenv("R_LIBS_USER")), file="")', '--vanilla', '--no-save', '--slave']
    path = None
    try:
        path = check_output(cmd, shell=False)
    except CalledProcessError:
        # FIXME: do something useful here
        raise
    return os.path.abspath(os.path.expanduser(path.strip()))


def check_r_libs_path():
    path = get_r_libs_user()
    if not os.path.exists(path):
        os.makedirs(path)


def init_work_env(species):
    check_r_libs_path()
    path = mkdtemp(dir=os.path.expanduser("~"))
    os.makedirs(os.path.join(path, 'species', species))
    os.makedirs(os.path.join(path, 'enviro', 'current'))
    # TODO: place data correctly
    return path


def write_bioclim_config(path, species):
    names = ["bioclim_01", "bioclim_04", "bioclim_05",
             "bioclim_06", "bioclim_12", "bioclim_15",
             "bioclim_16","bioclim_17"]

    params = {
        'workdir': path,
        'species': species,
        'occurence': os.path.join(path, 'species', species, 'occur.csv'),
        'background': "NULL", #os.path.join(path, 'species', species, 'background.csv')
        'enviro': {
            'names': ",".join( ('"{0}"'.format(name) for name in names) ),
            'data': ",".join( ('"{0}"'.format(os.path.join(path, 'enviro', 'current', name + ".asc")) for name in names) ),
            'type': ",".join( ('"continuous"' for i in xrange(0,len(names))))
            }
        }

    script = BIOCLIM_CONFIG.format(**params) + resource_string(org.bccvl.compute, 'rscripts/bioclim.R')
    scriptfile = os.path.join(path, 'bioclim.R')
    f = open(scriptfile,"w")
    f.write(script)
    f.close()
    return scriptfile


def prepare_data(path, names, climateitem, speciesitem):
    import pdb; pdb.set_trace()

    dest = open(os.path.join(path, 'enviro', 'current', 'current.zip'), 'w')
    src = climateitem.get('current_asc.zip').getFile().getBlob().open('r')
    shutil.copyfileobj(src, dest)
    src.close()
    dest.close()
    dest = open(os.path.join(path, 'species', 'ABT', 'occur.csv'), 'w')
    src = speciesitem.get('occur.csv').getFile().getBlob().open('r')
    shutil.copyfileobj(src, dest)
    src.close()
    dest.close()
    curzip = zipfile.ZipFile(os.path.join(path, 'enviro', 'current', 'current.zip'))
    curzip.extractall(path=os.path.join(path, 'enviro', 'current'))
    curzip.close()
    # for name in names:
    #     shutil.copy(os.path.join(path, 'enviro', 'current.76to05'), name + ".asc"),
    #                 os.path.join(path, 'enviro', 'current'))
    # shutil.copy(os.path.join(os.path.expanduser("~"), species, "occur.csv"),
    #             os.path.join(path, 'species', species))

def execute(context, climateitem, speciesitem):
    import ipdb; ipdb.set_trace()
    path = init_work_env("ABT")
    names = ["bioclim_01", "bioclim_04", "bioclim_05",
             "bioclim_06", "bioclim_12", "bioclim_15",
             "bioclim_16", "bioclim_17"]
    prepare_data(path, names, climatetime, speciesitem)
    script = write_bioclim_config(path, "ABT")
    scriptout = script + "out"
    cmd = ['R', 'CMD', 'BATCH', '--no-save', '--no-restore', script, scriptout]
    ret = call(cmd, shell=False)
    # TODO: check ret for error
    # TODO: make sure script returns proper error codes
    # TODO: zip result and store on context

if __name__ ==  '__main__':
    import ipdb; ipdb.set_trace()
    execute()
