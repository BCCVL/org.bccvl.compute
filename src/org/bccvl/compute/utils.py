"""
.. module:: utils
   :synopsis: Various little helper functions to be re-used within this
              package.

.. moduleauthor:: Gerhard Weis <g.weis@griffith.edu.au>
"""
import os
import os.path
from tempfile import mkdtemp
from plone.app.contenttypes.interfaces import IFile
from plone.i18n.normalizer.interfaces import IFileNameNormalizer
from zope.component import getUtility
from Products.CMFCore.utils import getToolByName
from urllib import urlopen
import shutil
import zipfile
import glob
from plone.namedfile.file import NamedBlobFile, NamedBlobImage


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
def prepare_data(path, names, climateitem, futureitem, speciesitem):
    # put datafiles onto filesystem
    for fileitem in climateitem.values():
        if not IFile.providedBy(fileitem):
            continue
        dest = open(os.path.join(path, 'enviro', fileitem.file.filename), 'w') # dexterity file has no filename
        src = fileitem.file.open('r')
        shutil.copyfileobj(src, dest)
        dest.close()
    for fileitem in futureitem.values():
        if not IFile.providedBy(fileitem):
            continue
        dest = open(os.path.join(path, 'enviro', fileitem.file.filename), 'w') # dexterity file has no filename
        src = fileitem.file.open('r')
        shutil.copyfileobj(src, dest)
        dest.close()
    destfolder = os.path.join(path, 'species', speciesitem.id)
    os.mkdir(destfolder)  # should not exist
    for fileitem in speciesitem.values():
        if not IFile.providedBy(fileitem):
            continue
        dest = open(os.path.join(destfolder, fileitem.file.filename), 'w')
        src = fileitem.file.open('r')
        shutil.copyfileobj(src, dest)
        dest.close()
    # unzip enviro data
    for zipfn in glob.glob(os.path.join(path, 'enviro', '*.zip')):
        with zipfile.ZipFile(zipfn) as curzip:
            curzip.extractall(path=os.path.join(path, 'enviro'))
        os.remove(zipfn)


def addFile(content, filename, file=None, mimetype='application/octet-stream'):
    normalizer = getUtility(IFileNameNormalizer)
    linkid = normalizer.normalize(os.path.basename(filename))
    if linkid in content:
        return content[linkid]
    if file is None:
        # TODO: add IStorage adapter for urllib.addinfourl see:
        # plone.namedfile-2.0.2-py2.7.egg/plone/namedfile/file.py:382: _setData(...)
        file = open(filename)
    if mimetype.startswith('image') and not 'tif' in mimetype:
        linkid = content.invokeFactory(type_name='Image', id=linkid,
                                       title=unicode(linkid))
        linkcontent = content[linkid]
        linkcontent.file = NamedBlobImage(contentType=mimetype, filename=unicode(linkid))
    else:
        linkid = content.invokeFactory(type_name='File', id=linkid,
                                       title=unicode(linkid))
        linkcontent = content[linkid]
        linkcontent.file = NamedBlobFile(contentType=mimetype, filename=unicode(linkid))
    linkcontent.setFormat(mimetype)
    linkcontent.file.data = file
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
    # FIXME: for some reason there is no auto id chooser here... maybe I'll have to setup
    #        some NameChooser adapter? so we have to generate a uinque id ourselves for now
    title = u'%s - result %s' % (experiment.title, datetime.now().isoformat())
    dsid = experiment.invokeFactory('gu.repository.content.RepositoryItem',
                                    id=title.encode('utf-8').replace(':', '-'),
                                    title=title)
    ds = experiment[dsid]
    # TODO: store experiment config here as well
    for fname in os.listdir(outdir):
        mtr = getToolByName(ds, 'mimetypes_registry')
        mtype = guess_mimetype(fname, outdir, mtr)
        addFile(ds,
                filename=os.path.join(outdir, fname),
                mimetype=mtype)
