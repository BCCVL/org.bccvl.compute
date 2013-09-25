"""
.. module:: utils
   :synopsis: Various little helper functions to be re-used within this
              package.

.. moduleauthor:: Gerhard Weis <g.weis@griffith.edu.au>
"""
import os
import os.path
from tempfile import mkdtemp
from org.bccvl.site.content.dataset import IDataset
from plone.i18n.normalizer.interfaces import IFileNameNormalizer
from zope.component import getUtility
from Products.CMFCore.utils import getToolByName
from urllib import urlopen
import shutil
import zipfile
import glob
from plone.namedfile.file import NamedBlobFile
from zope.event import notify
from zope.lifecycleevent import ObjectModifiedEvent
from plone.dexterity.utils import createContentInContainer


def check_r_libs_path(rootpath):
    """
    Create and return path to R library folder relative to given rootpath.
    If folder does not exist it will be created.

    :param rootpath: The top level folder to look for R/library sub-folder.
    :type rootpath: str

    :returns: full path to R library folder
    :rtype: str


    A little example how to use it
    >>> from tempfile import mkdtemp
    >>> import os.path
    >>> import shutil
    >>> root = mkdtemp()
    >>> path = check_r_libs_path(root)
    >>> path == os.path.join(root, 'R', 'library')
    True
    >>> os.path.isdir(path)
    True
    We have to clean up at the end.
    >>> shutil.rmtree(path)

    """
    path = os.path.join(rootpath, 'R', 'library')
    if not os.path.exists(path):
        os.makedirs(path)
    return path


def init_work_env(rootpath):
    """Prepare the folder structure for an R job.

    This method creates a new temporary work folder in rootpath, and
    creates all subfolders within it.

    :param rootpath: The work folder to put all data files and execute
    the R script within.
    :type rootpath: str

    :param species: The name of the sub-folder to hold the species data.
    :type species: str

    :returns: Full path to newly created work folder.
    :rtype: str

    A little example how to use it
    >>> from tempfile import mkdtemp
    >>> import os.path
    >>> import shutil
    >>> root = mkdtemp()
    >>> path = init_work_env(root)
    >>> os.path.isdir(path)
    True
    >>> os.path.isdir(os.path.join(path, 'species'))
    True
    >>> os.path.isdir(os.path.join(path, 'enviro'))
    True
    >>> os.path.isdir(path)
    We have to clean up at the end.
    >>> shutil.rmtree(root)

    """
    check_r_libs_path(rootpath)
    # FIXME: cleanup even if there is some serious error somewhere
    path = mkdtemp(dir=rootpath)
    os.makedirs(os.path.join(path, 'species'))
    os.makedirs(os.path.join(path, 'enviro'))
    # TODO: place data correctly
    return path

# TODO: ensure all file write/remove actions happen within tmp_dir (prefix check?)
def prepare_data(path, climateitem, futureitem, occurrenceitem, absenceitem):
    # put datafiles onto filesystem
    # Current climate Data
    dest = open(os.path.join(path, 'enviro', climateitem.file.filename), 'w') # dexterity file has no filename
    if climateitem is not None and IDataset.providedBy(climateitem):
        src = climateitem.file.open('r')
        shutil.copyfileobj(src, dest)
        dest.close()
    # Future climate data
    if futureitem is not None and IDataset.providedBy(futureitem):
        dest = open(os.path.join(path, 'enviro', futureitem.file.filename), 'w') # dexterity file has no filename
        src = futureitem.file.open('r')
        shutil.copyfileobj(src, dest)
        dest.close()
    # Species data
    # TODO: get the species id from somewhere
    destfolder = os.path.join(path, 'species', occurrenceitem.__parent__.id)
    os.mkdir(destfolder)  # should not exist
    if occurrenceitem is not None and IDataset.providedBy(occurrenceitem):
        #dest = open(os.path.join(destfolder, occurrenceitem.file.filename), 'w')
        #FIXME: get_sdm_params expects "occur.csv"
        dest = open(os.path.join(destfolder, "occur.csv"), 'w')
        src = occurrenceitem.file.open('r')
        shutil.copyfileobj(src, dest)
        dest.close()
    # FIXME: again assumes same id as for occurrence
    if absenceitem is not None and IDataset.providedBy(absenceitem):
        #dest = open(os.path.join(destfolder, absenceitem.file.filename), 'w')
        #FIXME: get_sdm_params expects "bkgd.csv"
        dest = open(os.path.join(destfolder, "bkgd.csv"), 'w')
        src = absenceitem.file.open('r')
        shutil.copyfileobj(src, dest)
        dest.close()
    # unzip enviro data
    for zipfn in glob.glob(os.path.join(path, 'enviro', '*.zip')):
        with zipfile.ZipFile(zipfn) as curzip:
            curzip.extractall(path=os.path.join(path, 'enviro'))
        os.remove(zipfn)


def get_datapath_for_glob(path, match):
    flist = list(glob.glob(os.path.join(path,  'enviro', match)))
    if len(flist):
        return flist[0]
    return None


def get_sdm_params(rootpath, path, species):
    # rootpath ... the worker home dir
    # path     ... the work dir
        # TODO: hardcoded list of bioclim variables
    names = ["bioclim_01", "bioclim_04", "bioclim_05",
             "bioclim_06", "bioclim_12", "bioclim_15",
             "bioclim_16", "bioclim_17"]
    # TODO: hardcoded sub-folder name in current climate data
    currentfolder = get_datapath_for_glob(path, 'current*')
    # TODO: hardcoded year for future projection
    futurefolder = get_datapath_for_glob(path, '*2085')
    # TODO: hardcoded file ending for raster data
    curdata = [os.path.join(currentfolder, name + ".tif") for name in names]
    futdata = None
    if futurefolder:
        # TODO: hardcoded file ending for raster data
        futdata = [os.path.join(futurefolder, name + ".tif") for name in names]
    bkgdata = None
    # TODO: assumes that prep data stores files bkgd.csv and occur.csv
    if os.path.exists(os.path.join(path, 'species', species, 'bkgd.csv')):
        bkgdata = os.path.join(path, 'species', species, 'bkgd.csv')

    params = {
        'rlibdir': check_r_libs_path(rootpath),
        'workdir': path,
        'species': species,
        'occurrence': os.path.join(path, 'species', species, 'occur.csv'),
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
    return params


# TODO: replace all stuff below with a transmogrifier pipeline
def addDataset(content, filename, file=None, mimetype='application/octet-stream'):
    normalizer = getUtility(IFileNameNormalizer)
    linkid = normalizer.normalize(os.path.basename(filename))
    if linkid in content:
        return content[linkid]
    if file is None:
        # TODO: add IStorage adapter for urllib.addinfourl see:
        # plone.namedfile-2.0.2-py2.7.egg/plone/namedfile/file.py:382: _setData(...)
        file = open(filename)
    linkid = content.invokeFactory(type_name='org.bccvl.content.dataset', id=linkid,
                                   title=unicode(linkid))
    linkcontent = content[linkid]
    linkcontent.file = NamedBlobFile(contentType=mimetype, filename=unicode(linkid))
    linkcontent.setFormat(mimetype)
    linkcontent.file.data = file

    notify(ObjectModifiedEvent(linkcontent))
    return linkcontent


from datetime import datetime
import mimetypes

# register a few mimetypes
mimetypes.add_type('text/plain', '.rascii')
mimetypes.add_type('text/plain', '.rout')
mimetypes.add_type('application/octet-stream', '.rdata')
mimetypes.add_type('image/geotiff', '.geotiff')


def guess_mimetype(name, path='', mtr=None):
    # 1. try mimetype registry
    mtype =  None
    if mtr is not None:
        mtype = mtr.lookupExtension(name)
        if mtype is not None:
            return mtype.normalized()
        # TODO: maybe try mtr(filecontents) to use MTRs mime magic
    if mtype is None:
        mtype = mimetypes.guess_type(name)
        # TODO: add mime magic here https://github.com/ahupp/python-magic/blob/master/magic.py
        if mtype is not (None, None):
            return mtype[0]
    return 'application/octet-stream'


def store_results(experiment, outdir):
    """
    create a new DataSet under experiment and store all files found in outdir
    within this dataset
    """
    # TODO: maybe use rfc822 date format?
    title = u'%s - result %s' % (experiment.title, datetime.now().isoformat())
    ds = createContentInContainer(experiment,
                                  'gu.repository.content.RepositoryItem',
                                  title=title)
    # TODO: store experiment config here as well
    for fname in os.listdir(outdir):
        mtr = getToolByName(ds, 'mimetypes_registry')
        mtype = guess_mimetype(fname, outdir, mtr)
        addDataset(ds,
                   filename=os.path.join(outdir, fname),
                   mimetype=mtype)
