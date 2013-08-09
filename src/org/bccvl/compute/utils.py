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
from urllib import urlopen
import shutil
import zipfile
import glob


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


def init_work_env(rootpath, species):
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
    >>> path = init_work_env(root, "ABC")
    >>> os.path.isdir(path)
    True
    >>> os.path.isdir(os.path.join(path, 'species', 'ABC'))
    True
    >>> os.path.isdir(os.path.join(path, 'enviro', 'current'))
    True
    >>> os.path.isdir(path)
    We have to clean up at the end.
    >>> shutil.rmtree(root)

    """
    check_r_libs_path(rootpath)
    # FIXME: cleanup even if there is some serious error somewhere
    path = mkdtemp(dir=rootpath)
    os.makedirs(os.path.join(path, 'species', species))
    os.makedirs(os.path.join(path, 'enviro', 'current'))
    # TODO: place data correctly
    return path


def prepare_data(path, names, climateitem, speciesitem):
    # put datafiles onto filesystem
    for fileitem in climateitem.values():
        if not IFile.providedBy(fileitem):
            continue
        dest = open(os.path.join(path, 'enviro', climateitem.file.filename), 'w')
        src = climateitem.file.getBlob().open('r')
        shutil.copyfileobj(src, dest)
        src.close()
        dest.close()
    destfolder = os.path.join(path, 'species', speciesitem.id)
    os.mkdir(destfolder)  # should not exist
    for speciesitem in speciesitem.values():
        if not IFile.providedBy(fileitem):
            continue
        dest = open(os.path.join(destfolder, speciesitem.file.filename), 'w')
        src = speciesitem.file.getBlob().open('r')
        shutil.copyfileobj(src, dest)
        src.close()
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
        file = urlopen(filename)
    linkid = content.invokeFactory(type_name='File', id=linkid,
                                   title=os.path.basename(filename),
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
    env = {'SERVER_NAME': 'fake_server',
           'SERVER_PORT': '80',
           'REQUEST_METHOD': 'GET'}
    request = HTTPRequest(sys.stdin, env, response)
    content.REQUEST = request
    linkcontent.processForm()
    del content.REQUEST  # Avoid "can't pickle file objects"

    return linkcontent
