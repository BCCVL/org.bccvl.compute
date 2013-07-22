import os
import os.path
import shutil
from tempfile import mkdtemp
from pkg_resources import resource_string
import zipfile
from plone.i18n.normalizer.interfaces import IFileNameNormalizer
from zope.component import getUtility
from urllib import urlopen

BIOCLIM_CONFIG="""
.libPaths("{rlibdir}")
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


def check_r_libs_path(rootpath):
    path = os.path.join(rootpath, 'R', 'library')
    if not os.path.exists(path):
        os.makedirs(path)
    return path


def init_work_env(rootpath, species):
    check_r_libs_path(rootpath)
    # FIXME: cleanup even if there is some serious error somewhere
    path = mkdtemp(dir=rootpath)
    os.makedirs(os.path.join(path, 'species', species))
    os.makedirs(os.path.join(path, 'enviro', 'current'))
    # TODO: place data correctly
    return path


def write_bioclim_config(rootpath, path, species):
    names = ["bioclim_01", "bioclim_04", "bioclim_05",
             "bioclim_06", "bioclim_12", "bioclim_15",
             "bioclim_16","bioclim_17"]

    params = {
        'rlibdir': check_r_libs_path(rootpath),
        'workdir': path,
        'species': species,
        'occurence': os.path.join(path, 'species', species, 'occur.csv'),
        'background': "NULL", #os.path.join(path, 'species', species, 'background.csv')
        'enviro': {
            'names': ",".join( ('"{0}"'.format(name) for name in names) ),
            'data': ",".join( ('"{0}"'.format(os.path.join(path, 'enviro', 'current', 'current.76to05', name + ".asc")) for name in names) ),
            'type': ",".join( ('"continuous"' for i in xrange(0,len(names))))
            }
        }

    script = BIOCLIM_CONFIG.format(**params) + resource_string('org.bccvl.compute', 'rscripts/bioclim.R')
    scriptfile = os.path.join(path, 'bioclim.R')
    f = open(scriptfile,"w")
    f.write(script)
    f.close()
    return scriptfile


def prepare_data(path, names, climateitem, speciesitem):

    dest = open(os.path.join(path, 'enviro', 'current', 'current.zip'), 'w')
    src = climateitem.getFile().getBlob().open('r')
    shutil.copyfileobj(src, dest)
    src.close()
    dest.close()
    dest = open(os.path.join(path, 'species', 'ABT', 'occur.csv'), 'w')
    src = speciesitem.getFile().getBlob().open('r')
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

def addFile(content, filename, file=None, mimetype='application/octet-stream'):
    normalizer = getUtility(IFileNameNormalizer)
    linkid = normalizer.normalize(os.path.basename(filename))
    if linkid in content:
        return content[linkid]
    if file is None:
        file = urlopen(filename)
    linkid = content.invokeFactory(type_name='File', id=linkid, title=os.path.basename(filename),
                                   file=file.read())
    linkcontent = content[linkid]
    linkcontent.setFormat(mimetype)
    linkcontent.setFilename(filename.encode('utf-8'))

    # FIXME: stupid archetypes ... do we really need to call processForm ?
    # Create a request to work with
    import sys
    from ZPublisher.HTTPResponse import HTTPResponse
    from ZPublisher.HTTPRequest import HTTPRequest
    response = HTTPResponse(stdout=sys.stdout)
    env = {'SERVER_NAME':'fake_server',
           'SERVER_PORT':'80',
           'REQUEST_METHOD':'GET'}
    request = HTTPRequest(sys.stdin, env, response)
    content.REQUEST = request
    linkcontent.processForm()
    del content.REQUEST #Avoid "can't pickle file objects"

    return linkcontent


def execute(context, climateitem, speciesitem):
    rootpath = os.environ.get('WORKER_DIR') or os.enivron['HOME']
    path = init_work_env(rootpath, "ABT")
    names = ["bioclim_01", "bioclim_04", "bioclim_05",
             "bioclim_06", "bioclim_12", "bioclim_15",
             "bioclim_16", "bioclim_17"]
    prepare_data(path, names, climateitem, speciesitem)
    script = write_bioclim_config(rootpath, path, "ABT")
    scriptout = script + "out"
    cmd = ['R', 'CMD', 'BATCH', '--no-save', '--no-restore', script, scriptout]
    ret = call(cmd, shell=False)
    # TODO: check ret for error
    # TODO: make sure script returns proper error codes
    # TODO: zip result and store on context
    with zipfile.ZipFile(os.path.join(path, 'output.zip'), 'w', zipfile.ZIP_DEFLATED) as zipf:
        for fname in os.listdir(os.path.join(path, 'output_bioclim')):
            zipf.write(os.path.join(path, 'output_bioclim', fname), fname)
        zipf.write(os.path.join(path, 'bioclim.Rout'), 'bioclim.Rout')
    addFile(context,
            filename=u'file://' + os.path.join(path, 'output.zip'),
            mimetype='application/zip')
    shutil.rmtree(path)


if __name__ ==  '__main__':
    execute()
