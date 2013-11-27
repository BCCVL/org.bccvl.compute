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
from decimal import Decimal
from plone.dexterity.utils import createContentInContainer
import transaction
from xmlrpclib import ServerProxy
from collective.transmogrifier.transmogrifier import Transmogrifier
from datetime import datetime
from plone.uuid.interfaces import IUUID
from plone.app.uuid.utils import uuidToObject
from org.bccvl.site.browser.xmlrpc import getbiolayermetadata
from rdflib import Namespace
import json
from plone.app.uuid.utils import uuidToObject
from persistent import Persistent
from zc.async.job import Job
from zc.async import local
from copy import deepcopy
from plone.app.async.interfaces import IAsyncService
from persistent.list import PersistentList
from plone.app.async.service import job_success_callback, job_failure_callback
from twisted.python.failure import Failure
from time import sleep
from pkg_resources import resource_filename

BIOCLIM = Namespace(u'http://namespaces.bccvl.org.au/bioclim#')


from StringIO import StringIO
from fabric import api
from fabric import tasks
from fabric import state
from fabric.contrib import files
from fabric import operations
from fabric import context_managers

import logging
# from zc.async.utils import tracelog
# tracelog.setLevel(logging.WARN)
LOG = logging.getLogger(__name__)

DATA_MOVER = 'http://127.0.0.1:10700/data_mover'

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


def run_script(cmd, workdir):
    # TODO: are these fabric context_managers thread safe?
    with context_managers.cd(workdir):
        result = api.run(cmd, pty=False, combine_stderr=False, quiet=True)
    return result
    # from fabric.operations import local
    # result = local(cmd, capture=True)
    # return result


def get_files_list(dir):
    result = api.run('find "{}" -type f'.format(dir),
                     pty=False, combine_stderr=False, quiet=True)
    return result


def decimal_encoder(o):
    if isinstance(o, Decimal):
        return float(o)
    raise TypeError(repr(o) + " is not JSON serializable")


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
        self.tmpparams = None
        self.scriptname = None
        self.wrapname = None

    def create_workdir(self):
        '''

        '''
        root = os.environ.get('WORKER_DIR') or os.environ['HOME']
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
            self.tmpimport = None
        if self.tmpscript:
            os.remove(self.tmpscript)
            self.tmpscript = None
        if self.tmpparams:
            os.remove(self.tmpparams)
            self.tmpparams = None

    def cleanup_remote(self):
        result = tasks.execute(cleanup_tmp, self.workdir, hosts=[self.host])
        self.jobs = []
        return result[self.host]

    def unpack(self, destdir, filename):
        result = tasks.execute(unpack, destdir, filename, hosts=[self.host])
        return result[self.host]

    def move_data(self, type, src, dest, uuid):
        s = ServerProxy(DATA_MOVER)
        job = s.move(src, dest)
        self.jobs.append({'type': type,
                          'filename': dest['path'],
                          'status': job,
                          'uuid': uuid})

    def move_input_data(self, type, datasetitem):
        if datasetitem is None or not IDataset.providedBy(datasetitem):
            return
        destname = '/'.join((self.inputdir, datasetitem.file.filename))
        src = {'type': 'url',
               'url': '{}/@@download/file/{}'.format(datasetitem.absolute_url(),
                                                     datasetitem.file.filename)}
        dest = {'host': 'plone', 'path': destname}
        self.move_data(type, src, dest, uuid=IUUID(datasetitem))

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

    def move_script(self, script, params):
        scriptfile, self.tmpscript = mkstemp()
        os.write(scriptfile, script)
        os.close(scriptfile)
        self.scriptname = '/'.join((self.scriptdir, 'sdm.R'))
        src = {'host': 'plone',
               'path': self.tmpscript}
        dest = {'host': 'plone',
                'path': self.scriptname}
        self.move_data('script', src, dest, None)

        paramsfile, self.tmpparams = mkstemp()
        paramsfile = os.fdopen(paramsfile, 'w')
        json.dump(params, paramsfile, default=decimal_encoder)
        paramsfile.close()
        self.paramsname = '/'.join((self.scriptdir, 'params.json'))
        src = {'host': 'plone',
               'path': self.tmpparams}
        dest = {'host': 'plone',
                'path': self.paramsname}
        self.move_data('script', src, dest, None)

        self.wrapname = '/'.join((self.scriptdir, 'wrap.sh'))
        src = {'host': 'plone',
               'path': resource_filename('org.bccvl.compute', 'rscripts/wrap.sh')}
        dest = {'host': 'plone',
                'path': self.wrapname}
        self.move_data('script', src, dest, None)

    def move_output_data(self):
        result = tasks.execute(get_files_list, self.outputdir, hosts=[self.host])
        files = [l for l in result[self.host].split('\n') if l]
        self.tmpimport = mkdtemp()
        for file in files:
            destname = '/'.join((self.tmpimport, file[len(self.outputdir) + 1:]))
            destdir = os.path.dirname(destname)
            if not os.path.exists(destdir):
                os.makedirs(destdir)
            src = {'host': 'plone',
                   'path': file}
            dest = {'host': 'plone',
                    'path': destname}
            self.move_data('output', src, dest, uuid=None)

    def import_output(self, experiment, jobid):
        # create result container which will be context for transmogrify import
        # TODO: maybe use rfc822 date format?
        title = u'%s - %s %s' % (experiment.title, jobid, datetime.now().isoformat())
        #LOG.info('Import result for %s from %s', title, self.workdir)
        ds = createContentInContainer(experiment,
                                      'gu.repository.content.RepositoryItem',
                                      title=title)
        # start transmogrify

        #transmogrify.dexterity.schemaupdater needs a REQUEST on context????
        from ZPublisher.HTTPResponse import HTTPResponse
        from ZPublisher.HTTPRequest import HTTPRequest
        import sys
        response = HTTPResponse(stdout=sys.stdout)
        env = {'SERVER_NAME': 'fake_server',
               'SERVER_PORT': '80',
               'REQUEST_METHOD': 'GET'}
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
        #LOG.info('Import result finished for %s from %s', title, self.workdir)

    def get_sdm_params(self):
        # TODO: hardcoded list of bioclim variables
        names = [BIOCLIM['B01'], BIOCLIM['B04'], BIOCLIM['B05'],
                 BIOCLIM['B06'], BIOCLIM['B12'], BIOCLIM['B15'],
                 BIOCLIM['B16'], BIOCLIM['B17']]
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
            # TODO: is this a generic sdm parameter?
            'tails': 'both',
            }
        for job in self.jobs:
            if job['type'] == 'current':
                ds = uuidToObject(job['uuid'])
                biometadata = getbiolayermetadata(ds)
                zipdir, _ = os.path.splitext(os.path.basename(job['filename']))
                currentfolder = '/'.join((self.inputdir, zipdir))
                params['enviro']['data'] = [os.path.join(currentfolder, biometadata[name]) for name in names]
            elif job['type'] == 'occurrence':
                params['occurrence'] = job['filename']
            elif job['type'] == 'absence':
                params['background'] = job['filename']
        return params

    def start_script(self):
        scriptout = os.path.join(self.outputdir,
                                 os.path.basename(self.scriptname + 'out'))

        # R --slave --vanilla -f $in_R --args $in_data > $out_data
        # R --slave --vanilla -f $in_R --args $in_data $out_data
        # -> Galaxy ... pass parameters as cli params
        # -> XQTL (Molgenis) ... wrap r-script with common init code, and provide special API to access parameters (maybe loaded from file in init wrapper?)
        cmd = 'nohup /bin/bash {} R CMD BATCH --vanilla --slave "{}" "{}" &'.format(self.wrapname, self.scriptname, scriptout)
        result = tasks.execute(run_script, cmd, self.scriptdir, hosts=[self.host])
        LOG.info("Remoe Job started %s", result)
        return result[self.host]

    def check_script(self):
        job_exit = os.path.join(self.scriptdir, 'job.exit')
        result = tasks.execute(files.exists, job_exit, hosts=[self.host])
        if not result:
            LOG.info("Remote file does not exist. %s", result)
            return None
        capture = StringIO()
        result = tasks.execute(operations.get, job_exit, capture, hosts=[self.host])
        LOG.info("Remote job finished (%s) with exit code %s", result, capture.getvalue())
        try:
            retval = int(capture.getvalue())
        except ValueError:
            # file did exist but didn't contain a valid number
            retval = -1
        return retval


def create_workenv(context, env, script, params):
    try:
        env.workdir = env.create_workdir()
        env.inputdir = '/'.join((env.workdir, 'input'))
        env.scriptdir = '/'.join((env.workdir, 'script'))
        env.outputdir = '/'.join((env.workdir, 'output'))
        env.mkdir(env.inputdir)
        env.mkdir(env.scriptdir)
        env.mkdir(env.outputdir)
        occurrence = uuidToObject(context.species_occurrence_dataset)
        absence = uuidToObject(context.species_absence_dataset)
        climate = uuidToObject(context.environmental_dataset)
        env.move_input_data('current', climate)
        env.move_input_data('occurrence', occurrence)
        env.move_input_data('absence', absence)
        params.update(env.get_sdm_params())
        env.move_script(script, params)
        env.wait_for_data_mover()
        env.unpack_enviro_data()
        return env
    except Exception as e:
        env.cleanup_remote()
        raise Exception(e, env)
    finally:
        env.cleanup()


def get_outputs(context, jobid, env):
    # TODO: result of previous job might be a zc.twist.Failure
    try:
        #LOG.info("getting outputs for %s", env.workdir)
        env.move_output_data()
        env.wait_for_data_mover()
        env.import_output(context, jobid)
        return env
    except Exception as e:
        raise Exception(e, env)
    finally:
        # TODO: don't delete here because transaction might need to be replayed or commit transaction above?'
        env.cleanup()
        #LOG.info("getting outputs cleaned up %s", env.workdir)


def run_job(context, env):
    # TODO: result of previous job might be a zc.twist.Failure
    try:
        env.start_script()
        while True:
            # TODO: make this some sort of time out and do proper error checking (e.g. connection failures)
            ret = env.check_script()
            if ret is None:
                LOG.info("Remote job net yet finished.")
                sleep(60)
                continue
            break
            # TODO: ret should be the exit status of R script
        return env
        # TODO: loop and wait for return status
        # What can go wrong?
        # ... polling for job might fail... temporary or machine lost?
        #     ... how to recover? ... cleanup might not happen
        # ... can't just remove everything, because other jobs might run
        #     could check for statuses of other jobs. (would need marker
        #     for  transfer there as well)
    except Exception as e:
        # Whatever went wrong we have to return something usefuel
        raise Exception(e, env)


def queue_job(experiment, jobid, env, script, params):
    try:
        # run import only if we run as async job
        async = getUtility(IAsyncService)
        queue = async.getQueues()['']

        # create processing jobs
        job1 = async.wrapJob((create_workenv, experiment, (), {'script': script, 'params': params}))
        job1.name = u'Transfering'
        job2 = async.wrapJob((run_job, experiment, (), {}))  # env as return value from job1
        job2.name = u'Running'
        job3 = async.wrapJob((get_outputs, experiment, (jobid, ), {}))  # env as return value from job2
        job3.name = u'Retrieving'

        main_job = async.wrapJob((finish_job_queue, experiment, (), {}))
        main_job.jobs = [job1, job2, job3]
        main_job.name = 'Cleanup'
        main_job.jobid = jobid
        main_job.addCallbacks(success=job_success_callback,
                              failure=job_failure_callback)
        #start_job = async.wrapJob((queue_next, experiment, (main_job, 0, env), {}))
        start_job = Job(queue_next, main_job, 0, env)
        start_job.quota_names = ('default', )
        queue.put(start_job)
        main_job.annotations['bccvl.status'] = {
            'step': 0,
            'steps': len(main_job.jobs),
            'history': [],
            'task': u'New'}
        return main_job

    except Exception, e:
        env.cleanup()
        env.cleanup_remote()
        raise e


def finish_job_queue(context, result):
    # FIXME: need to handle errors here
    main_job = local.getJob()
    newstate = u'Failed'
    if isinstance(result, WorkEnv):
        # clenup remote and local
        env = result
        newstate = u'Completed'
    else:
        ex, env = result.value.args
        # where do I get env from?
    env.cleanup_remote()
    env.cleanup()
    status = main_job.annotations['bccvl.status']
    status = deepcopy(status)
    status['task'] = newstate
    main_job.annotations['bccvl.status'] = status
    return result


# used as callback on sub jobs. job is main_job which keeps track of progress
#def queue_next(context, main_job, ix, result):
def queue_next(main_job, ix, result):
    status = main_job.annotations.get('bccvl.status')
    #status = local.getLiveAnnotation('bccvl.status', job=main_job)
    status = deepcopy(status)
    queue = local.getQueue()
    if queue is None:
        # try to get queue form parent job
        queue = local.getJob().parent.parent.queue
    # if prev:
    #     status['history'].append({
    #         'start': prev.active_start,
    #         'end': prev.active_end,
    #         'status': prev.status})

    # FIXME: ask for WorkEnv? instead of Failure?
    if ix < len(main_job.jobs) and not isinstance(result, Failure):
        next = main_job.jobs[ix]
        next.kwargs['env'] = result
        status['step'] += 1
        status['task'] = next.name
        main_job.annotations['bccvl.status'] = status
        #local.setLiveAnnonation('bccvl.status', status, job=main_job)
        #qjob = async.wrapJob((queue_next, context, (main_job, ix+1), {}))
        qjob = Job(queue_next, main_job, ix + 1)
        next.addCallbacks(qjob, qjob)
        return queue.put(next)
    else:
        status['task'] = main_job.name
        main_job.annotations['bccvl.status'] = status
        #local.setLiveAnnonation('bccvl.status', status, job=main_job)
        main_job.args.append(result)
        return queue.put(main_job)


class WorkEnvLocal(WorkEnv):

    def create_workdir(self):
        # FIXME: remove this one
        # workdir = '/tmp/rwork'
        # if os.path.exists(workdir):
        #     shutil.rmtree(workdir)
        # os.mkdir(workdir)

        root = os.environ.get('WORKER_DIR') or os.environ['HOME']
        workdir = mkdtemp(dir=root)
        return workdir

    def move_data(self, type, src, dest, uuid):
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
                          'status': job,
                          'uuid': uuid})

    def move_input_data(self, type, datasetitem):
        if datasetitem is None or not IDataset.providedBy(datasetitem):
            return
        destname = '/'.join((self.inputdir, datasetitem.file.filename))
        src = {'type': 'blob',
               'file': datasetitem.file.open()}
        dest = {'host': 'plone', 'path': destname}
        self.move_data(type, src, dest, uuid=IUUID(datasetitem))

    def wait_for_data_mover(self):
        # all local copy no wait here
        pass
