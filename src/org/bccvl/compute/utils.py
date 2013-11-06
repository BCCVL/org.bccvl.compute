"""
.. module:: utils
   :synopsis: Various little helper functions to be re-used within this
              package.

.. moduleauthor:: Gerhard Weis <g.weis@griffith.edu.au>
"""
import os
import os.path
from tempfile import mkdtemp, mkstemp
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
import transaction
from xmlrpclib import ServerProxy
from collective.transmogrifier.transmogrifier import Transmogrifier
from datetime import datetime


from StringIO import StringIO
from fabric import api
from fabric import tasks
from fabric import state

DATA_MOVER = 'http://127.0.0.1:6543/data_mover'

##TASKS:


def create_tmpdir(root=''):
    '''
    Create a new temporary directory on target hosts
    '''
    result = api.run("mktemp -d -t rworkdir {}".format(root),
                     pty=False, combine_stderr=False, quiet=True)
    return result


def mkdir(dir):
    '''
    '''
    result = api.run("mkdir {}".format(dir),
                     pty=False, combine_stderr=False, quiet=True)
    return result


def cleanup_tmp(tmpdir):
    if not tmpdir or tmpdir in ('/'):
        raise ValueError("Invalid dir to clean up.", tmpdir)
    result = api.run("rm -fr {}".format(tmpdir),
                     pty=False, combine_stderr=False, quiet=True)
    return result


def unpack(destdir, filename):
    result = api.run("unzip -d '{}' {}".format(destdir, filename),
                     pty=False, combine_stderr=False, quiet=True)
    return result


def run_script(cmd):
    result = api.run(cmd, pty=False, combine_stderr=False, quiet=True)
    return result
    # from fabric.operations import local
    # result = local(cmd, capture=True)
    # return result


def get_files_list(dir):
    result = api.run('find "{}" -type f'.format(dir),
                     pty=False, combine_stderr=False, quiet=True)
    return result


## JOB-UTILS:
##    use fabric tasks to do stuff
class WorkEnv(object):

    def __init__(self, host):
        self.host = host
        self.workdir = None
        self.inputdir = None
        self.scriptdir = None
        self.outputdir = None
        self.jobs = []
        self.tmpimport = None
        self.tmpscript = None
        self.scriptname = None

    def create_workdir(self, root=''):
        '''

        '''
        result = tasks.execute(create_tmpdir, root, hosts=[self.host])
        workdir = result['localhost']
        # TODO: check result status
        # workdir.failed=bool
        # workdir.succeeded=bool
        # workdir.return_code= int
        return workdir

    def mkdir(self, dir):
        result = tasks.execute(mkdir, dir, hosts=[self.host])
        return result[self.host]

    def cleanup(self):
        if self.tmpimport:
            shutil.rmtree(self.tmpimport)
        if self.tmpscript:
            os.remove(self.tmpscript)
        result = tasks.execute(cleanup_tmp, self.workdir, hosts=[self.host])
        return result[self.host]

    def unpack(self, destdir, filename):
        result = tasks.execute(unpack, destdir, filename, hosts=[self.host])
        return result[self.host]

    def move_data(self, type, src, dest):
        s = ServerProxy(DATA_MOVER)
        job = s.move(src, dest)
        self.jobs.append({'type': type,
                          'filename': dest['path'],
                          'status': job})

    def move_input_data(self, type, datasetitem):
        if datasetitem is None or not IDataset.providedBy(datasetitem):
            return
        destname = '/'.join((self.inputdir, datasetitem.file.filename))
        src = {'type': 'url',
               'url': '{}/@@download/file/{}'.format(datasetitem.absolute_url(),
                                                     datasetitem.file.filename)}
        dest = {'host': 'plone', 'path': destname}
        self.move_data(type, src, dest)

    def wait_for_data_mover(self):
        import time
        stillgoing = True
        s = ServerProxy(DATA_MOVER)
        while stillgoing:
            time.sleep(1)
            stillgoing = False
            for job in self.jobs:
                job['status'] = s.check_move_status(job['status']['id'])
                if job['status'] in ('PENDING', 'IN_PROGRESS'):
                    stillgoing = True
                    break

    def unpack_enviro_data(self):
        for job in self.jobs:
            if job['type'] in ('current', 'future'):
                dirname, _ = os.path.splitext(os.path.basename(job['filename']))
                destdir = '/'.join((self.inputdir, dirname))
                self.unpack(destdir, job['filename'])

    def move_script(self, script):
        scriptfile, self.tmpscript = mkstemp()
        os.write(scriptfile, script)
        os.close(scriptfile)
        self.scriptname = '/'.join((self.scriptdir, 'sdm.R'))
        src = {'host': 'plone',
               'path': self.tmpscript}
        dest = {'host': 'plone',
                'path': self.scriptname}
        self.move_data('script', src, dest)

    def execute_script(self):
        scriptout = os.path.join(self.outputdir,
                                 os.path.basename(self.scriptname + 'out'))

        # R --slave --vanilla -f $in_R --args $in_data > $out_data
        # R --slave --vanilla -f $in_R --args $in_data $out_data
        # -> Galaxy ... pass parameters as cli params
        # -> XQTL (Molgenis) ... wrap r-script with common init code, and provide special API to access parameters (maybe loaded from file in init wrapper?)
        cmd = 'R CMD BATCH --vanilla --slave "{}" "{}"'.format(self.scriptname, scriptout)
        result = tasks.execute(run_script, cmd, hosts=[self.host])
        return result[self.host]

    def move_output_data(self):
        result = tasks.execute(get_files_list, self.outputdir, hosts=[self.host])
        files = [l for l in result[self.host].split('\n') if l]
        self.tmpimport = mkdtemp()
        for file in files:
            destname = '/'.join((self.tmpimport, file[len(self.outputdir) + 1:]))
            src = {'host': 'plone',
                   'path': file}
            dest = {'host': 'plone',
                    'path': destname}
            self.move_data('output', src, dest)

    def import_output(self, experiment) :
        # try to avoid DB conflicts by committing first
        transaction.commit()
        # create result container which will be context for transmogrify import
        # TODO: maybe use rfc822 date format?
        title = u'%s - result %s' % (experiment.title, datetime.now().isoformat())
        ds = createContentInContainer(experiment,
                                      'gu.repository.content.RepositoryItem',
                                      title=title)
        # start transmogrify

        #transmogrify.dexterity.schemaupdater needs a REQUEST on context????
        from ZPublisher.HTTPResponse import HTTPResponse
        from ZPublisher.HTTPRequest import HTTPRequest
        import sys
        response = HTTPResponse(stdout=sys.stdout)
        env = {'SERVER_NAME':'fake_server',
               'SERVER_PORT':'80',
               'REQUEST_METHOD':'GET'}
        request = HTTPRequest(sys.stdin, env, response)
        # Set values from original request
        # original_request = kwargs.get('original_request')
        # if original_request:
        #     for k,v in original_request.items():
        #       request.set(k, v)
        ds.REQUEST = request

        transmogrifier = Transmogrifier(ds)
        transmogrifier(u'org.bccvl.compute.resultimport',
                       resultsource={'path': self.tmpimport})

        # cleanup fake request
        del ds.REQUEST

    def prepare_work_env(self, climateitem, occurrenceitem, absenceitem):
        self.workdir = self.create_workdir()
        self.inputdir = '/'.join((self.workdir, 'input'))
        self.scriptdir = '/'.join((self.workdir, 'script'))
        self.outputdir = '/'.join((self.workdir, 'output'))
        self.mkdir(self.inputdir)
        self.mkdir(self.scriptdir)
        self.mkdir(self.outputdir)
        self.move_input_data('current', climateitem)
        self.move_input_data('occurrence', occurrenceitem)
        self.move_input_data('absence', absenceitem)

    def get_sdm_params(self):
        # TODO: hardcoded list of bioclim variables
        names = ["bioclim_01", "bioclim_04", "bioclim_05",
                 "bioclim_06", "bioclim_12", "bioclim_15",
                 "bioclim_16", "bioclim_17"]
        params = {
            'scriptdir': self.workdir,
            'inputdir': self.inputdir,
            'outputdir': self.outputdir,
            'occurrence': None,
            'background': None,
            'enviro': {
                'names': names,
                'data': None,
                'type': ["continuous" for i in xrange(0, len(names))],
                },
            'tails': 'both',
            }
        for job in self.jobs:
            if job['type'] == 'current':
                # FIXME: hardcoded zip path name
                zipdir, _ = os.path.splitext(os.path.basename(job['filename']))
                currentfolder = '/'.join((self.inputdir, zipdir, 'current.76to05'))
                params['enviro']['data'] = [os.path.join(currentfolder, name + ".tif") for name in names]
            elif job['type'] == 'occurrence':
                params['occurrence'] = job['filename']
            elif job['type'] == 'absence':
                params['background'] = job['filename']
        return params

    def execute(self, script):
        self.move_script(script)
        self.wait_for_data_mover()
        self.unpack_enviro_data()
        self.execute_script()
        self.move_output_data()
        self.wait_for_data_mover()


class WorkEnvLocal(WorkEnv):

    def create_workdir(self, root=''):
        # FIXME: remove this one
        workdir = '/tmp/rwork'
        if os.path.exists(workdir):
            shutil.rmtree(workdir)
        os.mkdir(workdir)
        return workdir

    def move_data(self, type, src, dest):
        if src.get('host', None):
            print "copy", src['path'], dest['path']
            shutil.copyfile(src['path'], dest['path'])
        elif src['type'] == 'host':
            shutil.copyfile(src['path'], dest['path'])
        elif src['type'] == 'blob':
            srcfile = src['file']
            destfile = open(dest['path'], 'w')
            shutil.copyfileobj(srcfile, destfile)
        job = {'status': 'COMPLETED'}
        self.jobs.append({'type': type,
                          'filename': dest['path'],
                          'status': job})

    def move_input_data(self, type, datasetitem):
        if datasetitem is None or not IDataset.providedBy(datasetitem):
            return
        destname = '/'.join((self.inputdir, datasetitem.file.filename))
        src = {'type': 'blob',
               'file': datasetitem.file.open()}
        dest = {'host': 'plone', 'path': destname}
        self.move_data(type, src, dest)

    def wait_for_data_mover(self):
        # all local copy no wait here
        pass

    def move_script(self, script):
        scriptfile, self.tmpscript = mkstemp()
        os.write(scriptfile, script)
        os.close(scriptfile)
        self.scriptname = '/'.join((self.scriptdir, 'sdm.R'))
        src = {'host': 'plone',
               'path': self.tmpscript}
        dest = {'host': 'plone',
                'path': self.scriptname}
        self.move_data('script', src, dest)
