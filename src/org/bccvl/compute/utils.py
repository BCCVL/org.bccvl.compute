"""
.. module:: utils
   :synopsis: Various little helper functions to be re-used within this
              package.

.. moduleauthor:: Gerhard Weis <g.weis@griffith.edu.au>
"""
from itertools import chain
import os
import os.path
from tempfile import mkdtemp, mkstemp
from org.bccvl.site.content.dataset import IDataset
from zope.component import getUtility
import shutil
from decimal import Decimal
from xmlrpclib import ServerProxy
from collective.transmogrifier.transmogrifier import Transmogrifier
from plone.uuid.interfaces import IUUID
from plone.app.uuid.utils import uuidToObject
from org.bccvl.site.browser.xmlrpc import getbiolayermetadata
import json
from zc.async.job import Job
from zc.async import local
from plone.app.async.interfaces import IAsyncService
from plone.app.async.service import job_success_callback, job_failure_callback
from twisted.python.failure import Failure
from time import sleep
from pkg_resources import resource_filename
import transaction
import transaction.interfaces

import paramiko

import logging
from zc.async.utils import tracelog
tracelog.setLevel(logging.WARN)
LOG = logging.getLogger(__name__)

DATA_MOVER = 'http://127.0.0.1:10700/data_mover'
COMPUTE_HOST = 'localhost'
COMPUTE_USER = 'bccvl'
INTERNAL_URL = 'http://127.0.0.1:8201'
#COMPUTE_IP = '130.102.155.47'

##TASKS:


class SSHTool(object):

    _client = None

    def __init__(self, host, user=None):
        self.user = user
        self.host = host

    @property
    def client(self):
        if self._client is None:
            LOG.info("SSH Connect to %s@%s", self.user, self.host)
            client = paramiko.SSHClient()
            client.load_system_host_keys()
            client.set_missing_host_key_policy(paramiko.WarningPolicy())
            client.connect(hostname=self.host, username=self.user)
            self._client = client
        return self._client

    def close(self):
        if self._client:
            self._client.close()
        self._client = None

    def _run(self, cmd):
        stdin, stdout, stderr = self.client.exec_command(cmd)
        retcode = stdout.channel.recv_exit_status()
        LOG.info("Remote Command: %s", cmd)
        LOG.info("      code: %s", retcode)
        stdout = stdout.read()
        LOG.info("    stdout: %s", stdout)
        LOG.info("    stderr: %s", stderr.read())
        return (retcode, stdout)

    def create_tmpdir(self, root=''):
        '''
        Create a new temporary directory on target hosts
        '''
        cmd = "TMPDIR='{}' mktemp -d -t rworkdir.XXXXXX".format(root)
        code, stdout = self._run(cmd)
        return code, stdout

    def check_pid(self, pid):
        cmd = "kill -0 '{}'".format(pid)
        code, stdout = self._run(cmd)
        return code, stdout

    def mkdir(self, dir):
        '''
        '''
        cmd = "mkdir '{}'".format(dir)
        code, stdout = self._run(cmd)
        return code, stdout

    def cleanup_tmp(self, tmpdir):
        if not tmpdir or tmpdir in ('/'):
            # import ipdb; ipdb.set_trace()
            raise ValueError("Invalid dir to clean up.", tmpdir)
        cmd = "rm -fr '{}'".format(tmpdir)
        code, stdout = self._run(cmd)
        return code, stdout

    def unpack(self, destdir, filename):
        cmd = "unzip -o -d '{}' '{}'".format(destdir, filename)
        code, stdout = self._run(cmd)
        return code, stdout

    def run_script(self, cmd, workdir):
        # TODO: are these fabric context_managers thread safe?
        cmd = "cd '{}' </dev/null >/dev/null 2>&1 ; {}".format(workdir, cmd)
        code, stdout = self._run(cmd)
        return code, stdout

    def get_files_list(self, dir):
        cmd = "find '{}' -type f".format(dir)
        code, result = self._run(cmd)
        return code, result

    def file_exists(self, path):
        cmd = "test -e '{}'".format(path)
        code, result = self._run(cmd)
        return not code

    def read_file(self, path):
        sftp = self.client.open_sftp()
        data = None
        try:
            file = sftp.open(path)
            data = file.read()
        except:
            LOG.info("File transfer %s failed.", path)
        return data


def decimal_encoder(o):
    if isinstance(o, Decimal):
        return float(o)
    raise TypeError(repr(o) + " is not JSON serializable")


class WorkEnv(object):

    _ssh = None

    def __init__(self, host=None):
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
        self.jobid = None
        self.src = 'plone'
        self.dest = 'compute'
        if host is None:
            self.host = os.environ.get('COMPUTE_HOST', COMPUTE_HOST)
        self.user = os.environ.get('COMPUTE_USER', COMPUTE_USER)
        self.data_mover = os.environ.get('DATA_MOVER', DATA_MOVER)

    @property
    def ssh(self):
        if self._ssh is None:
            self._ssh = SSHTool(self.host, self.user)
        return self._ssh

    def close(self):
        if self._ssh is not None:
            self._ssh.close()
        self._ssh = None

    def create_workdir(self):
        '''

        '''
        root = os.environ.get('WORKER_DIR') or os.environ['HOME']
        code, workdir = self.ssh.create_tmpdir(root)
        # TODO: check errors:
        return workdir.strip()

    def mkdir(self, dir):
        code, result = self.ssh.mkdir(dir)
        return result

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
        code, result = self.ssh.cleanup_tmp(self.workdir)
        self.jobs = []
        return result

    def unpack(self, destdir, filename):
        code, result = self.ssh.unpack(destdir, filename)
        return result

    def move_data(self, type, src, dest, uuid):
        retry = 5
        while retry:
            try:
                s = ServerProxy(self.data_mover)
                job = s.move(src, dest)
                LOG.info("Submitted move job to data_mover: %s", job)
                self.jobs.append({'type': type,
                                  'filename': dest['path'],
                                  'status': job,
                                  'uuid': uuid})
                return
            except Exception as e:
                retry -= 1
                LOG.info("Datamover error for job %s retry: %d. %s",
                         dest, retry, e)
                sleep(5)
        raise Exception("Datamover communication failed")

    def move_input_data(self, type, dsinfo):
        # datasetinfo['filename']
        # datasetinfo['url']
        # TODO: do this check somewhere?
        # if datasetitem is None or not IDataset.providedBy(datasetitem):
        #     return
        # TODO: take care of non unique destination names ... dataset uuid?
        destname = '/'.join((self.inputdir, dsinfo['filename']))
        src = {'type': 'url',
               'url': dsinfo['url']}
        dest = {'type': 'scp',
                'host': self.dest,
                'path': destname}
        self.move_data(type, src, dest, uuid=dsinfo['uuid'])

    def wait_for_data_mover(self):
        # TODO: Time_out, and failure handling
        import time
        stillgoing = True
        s = ServerProxy(self.data_mover)
        while stillgoing:
            LOG.info('check data_mover after sleep')
            time.sleep(10)
            stillgoing = False
            for job in self.jobs:
                moveid = job['status']['id']
                if job['status']['status'] in ('COMPLETED', 'FAILED'):
                    LOG.info("Skip finished move job %s",
                             job['status']['status'])
                    continue
                job['status'] = s.check_move_status(job['status']['id'])
                LOG.info("DataMover: %s %s", moveid,  job['status'])
                if job['status']['status'] in ('PENDING', 'IN_PROGRESS'):
                    stillgoing = True
                    break
                elif job['status']['status'] == 'FAILED':
                    LOG.fatal('File transfer failed: %s', job)
                else:
                    LOG.info("Move Job %s %s", job['status'], job['filename'])
                    if job['status']['status'] != 'COMPLETED':
                        LOG.fatal('Unknown data_mover return status:  %s',
                                  job['status'])
                # TODO: otherwise done and could remove job from list

    def unpack_enviro_data(self):
        for job in self.jobs:
            if job['filename'].endswith('.zip'):
                dirname, _ = os.path.splitext(os.path.basename(job['filename']))
                destdir = '/'.join((self.inputdir, dirname))
                self.unpack(destdir, job['filename'])

    def move_script(self, script, params):
        scriptfile, self.tmpscript = mkstemp()
        os.write(scriptfile, script)
        os.close(scriptfile)
        # FIXME: need more sophisticated way to do the right thing:
        if 'perl' in script.split('\n')[0]:
            self.scriptname = '/'.join((self.scriptdir, '{}.pl'.format(self.jobid)))
        else:
            self.scriptname = '/'.join((self.scriptdir, '{}.R'.format(self.jobid)))
        src = {'type':  'scp',
               'host': self.src,
               'path': self.tmpscript}
        dest = {'type': 'scp',
                'host': self.dest,
                'path': self.scriptname}
        self.move_data('script', src, dest, None)

        paramsfile, self.tmpparams = mkstemp()
        paramsfile = os.fdopen(paramsfile, 'w')
        json.dump(params, paramsfile, default=decimal_encoder,
                  sort_keys=True, indent=4)
        paramsfile.close()
        self.paramsname = '/'.join((self.scriptdir, 'params.json'))
        src['path'] = self.tmpparams
        dest['path'] = self.paramsname
        self.move_data('script', src, dest, None)

        self.wrapname = '/'.join((self.scriptdir, 'wrap.sh'))
        src['path'] = resource_filename('org.bccvl.compute',
                                        'rscripts/wrap.sh')
        dest['path'] = self.wrapname
        self.move_data('script', src, dest, None)

        # TODO: We should only copy this if it's needed.
        # TODO: In bccvl.R, the working directory is configured to the output directory, hence we place the maxent.jar file there...
        #       This means that we get copies of this file in the output of each experiment.

    def move_output_data(self):
        # TODO: use data_mover's transfer folder feature?'
        code, result = self.ssh.get_files_list(self.outputdir)
        files = [l for l in result.split('\n') if l]
        self.tmpimport = mkdtemp()
        for file in files:
            destname = '/'.join((self.tmpimport,
                                 file[len(self.outputdir) + 1:]))
            destdir = os.path.dirname(destname)
            if not os.path.exists(destdir):
                os.makedirs(destdir)
            src = {'type': 'scp',
                   'host': self.dest,
                   'path': file}
            dest = {'type': 'scp',
                    'host': self.src,
                    'path': destname}
            self.move_data('output', src, dest, uuid=None)

    def import_output(self, result, workenv, OUTPUTS):
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

        retry = 5
        while retry:
            try:
                transaction.begin()  # resync

                result.REQUEST = request
                transmogrifier = Transmogrifier(result)
                transmogrifier(u'org.bccvl.compute.resultimport',
                               resultsource={'path': self.tmpimport,
                                             'outputmap': OUTPUTS})
                # cleanup fake request
                del result.REQUEST
                transaction.commit()
                retry = 0
            except transaction.interfaces.TransientError:
                # Catch conflict errors
                transaction.abort()
                retry -= 1
                LOG.info("Retrying IMPORT: %s attempts left", retry)

        LOG.info('Import result finished for %s from %s', result.title, self.workdir)

    def get_sdm_params(self, experimentinfo):
        # FIXME: move tis away
        #  prepare files for exec environment. e.g. find workenv local filenames for input files, etc...
        names = set(chain(*experimentinfo.get('layers', {}).values()))
        params = {
            'scriptdir': self.workdir,
            'inputdir': self.inputdir,
            'outputdir': self.outputdir,
            # TODO: all the parameters below must go into sdm modules
            'environmenttype': ["continuous" for i in xrange(0, len(names))],
            # 'enviro': {
            #     'names': names,
            #     'data': [],
            #     'type':
            #     },
            # TODO: is this a generic sdm parameter?
            'tails': 'both',
            }
        # TODO:  split experimentinfo / params so that params is part
        # of experimentinfo and contais only parameters passed no to
        # script. experimentinfo itself will be a container for info
        # about experiment (incl. parameters)
        jobbyuid = {}
        for job in self.jobs:
            if job['type'] not in params:
                params[job['type']] = []  # TODO: document ...
                                          # job['type'] is name of parameter
            #params[job['type']].append(job)
            jobbyuid[job['uuid']] = job
        # build up enviro.data list
        # TODO: find layernames in case we extracted a zip
        #       code assumes we have layers, to associate, but
        #       this code should do generic unzip file replacement
        zipfile = set()
        if 'layers' in experimentinfo:
            for dsuuid, layers in experimentinfo['layers'].items():
                zipdir, _ = os.path.splitext(os.path.basename(jobbyuid[dsuuid]['filename']))
                currentfolder = '/'.join((self.inputdir, zipdir))
                expmd = experimentinfo[jobbyuid[dsuuid]['type']][dsuuid]
                for layer in layers:
                    filename = os.path.join(currentfolder,
                                            expmd['layers'][layer])
                    params[jobbyuid[dsuuid]['type']].append(filename)
                zipfile.add(dsuuid)
        if 'layersperdataset' in experimentinfo:
            for paramname in experimentinfo['datasetkeys']:
                dslist = {}
                for job in self.jobs:
                    if job['type'] != paramname:
                        continue
                    expmd = experimentinfo[job['type']][job['uuid']]
                    if not 'layers' in expmd or not expmd['layers']:
                        continue
                    zipfile.add(job['uuid'])
                    zipdir, _ = os.path.splitext(os.path.basename(job['filename']))
                    currentfolder = '/'.join((self.inputdir, zipdir))
                    for layer in experimentinfo['layersperdataset']:
                        filename = os.path.join(currentfolder,
                                                expmd['layers'][layer])
                        if job['uuid'] not in dslist:
                            dslist[job['uuid']] = []
                        dslist[job['uuid']].append(filename)
                if dslist:
                    params[paramname] = dslist.values()
        for job in self.jobs:
            if job['uuid'] in zipfile:
                continue
            params[job['type']].append(job['filename'])
        return params

    def start_script(self):
        scriptout = os.path.join(self.outputdir,
                                 os.path.basename(self.scriptname + 'out'))

        # R --slave --vanilla -f $in_R --args $in_data > $out_data
        # R --slave --vanilla -f $in_R --args $in_data $out_data
        # -> Galaxy ... pass parameters as cli params
        # -> XQTL (Molgenis) ... wrap r-script with common init code,
        #        and provide special API to access parameters (maybe
        #        loaded from file in init wrapper?)
        # FIXME: better way to decide how to run script
        if self.scriptname.endswith('.R'):
            cmd = 'nohup /bin/bash --login {} R CMD BATCH --vanilla "{}" "{}" </dev/null >/dev/null 2>&1 &'.format(self.wrapname, self.scriptname, scriptout)
        else:
            cmd = 'nohup /bin/bash --login {} perl {} < /dev/null > {} 2>&1 &'.format(self.wrapname, self.scriptname, scriptout)
        code, result = self.ssh.run_script(cmd, self.scriptdir)
        LOG.info("Remoe Job started %s", result)
        return result

    def check_script(self):
        job_exit = os.path.join(self.scriptdir, 'job.exit')
        pidfile = os.path.join(self.scriptdir, 'job.pid')
        result = self.ssh.file_exists(job_exit)
        retval = -1
        if not result:
            LOG.info("Remote file does not exist. %s", result)
            result = self.ssh.file_exists(pidfile)
            if not result:
                LOG.info("PID file doesn't exist. Job failed to start.")
                return -1
            else:
                capture = self.ssh.read_file(pidfile)
                if not capture:
                    LOG.info("PID file possibly empty.")
                    return -1
                code, result = self.ssh.check_pid(capture.strip())
                if code != 0:
                    LOG.info("Process no longer running, but job.exit file not generated.")
                    return - 1
                LOG.info("Process still running")
            return None
        capture = self.ssh.read_file(job_exit)
        LOG.info("Remote job finished (%s) with exit code %s", result, capture)
        try:
            retval = int(capture.strip())
        except ValueError:
            # file did exist but didn't contain a valid number
            LOG.fatal("Couldn't parse remote exit code")
            retval = -1
        return retval


def getdatasetparams(uuid):
    dsobj = uuidToObject(uuid)
    if dsobj is None:
        return {}
    dsinfo = getDatasetInfo(dsobj)
    dsinfo['uuid'] = uuid
    # TODO: not all datasets have layers
    dsinfo['layers'] = dict(((k, v['filename']) for
                             k, v in getbiolayermetadata(dsobj).items()))
    return dsinfo


def create_workenv(env, script, params):
    try:
        env.workdir = env.create_workdir()
        env.inputdir = '/'.join((env.workdir, 'input'))
        env.scriptdir = '/'.join((env.workdir, 'script'))
        env.outputdir = '/'.join((env.workdir, 'output'))
        env.mkdir(env.inputdir)
        env.mkdir(env.scriptdir)
        env.mkdir(env.outputdir)
        # TODO: this supports files only in top level dict
        for dskey in params['datasetkeys']:
            for dsinfo in params[dskey].values():
                if 'url' in dsinfo:
                    # TODO: no guarantee that inputfile name is unique
                    #       use dataset uuid?
                    env.move_input_data(dskey, dsinfo)
        params.update(env.get_sdm_params(params))
        env.move_script(script, params)
        env.wait_for_data_mover()
        env.unpack_enviro_data()
        return env
    except Exception as e:
        import traceback
        LOG.fatal('create_workenv: %s', traceback.format_exc())
        env.cleanup_remote()
        raise Exception(str(e), env)
    finally:
        env.cleanup()
        # cleanup fabric state
        #env.close()


def get_outputs(context, env, OUTPUTS):
    # TODO: result of previous job might be a zc.twist.Failure
    try:
        #LOG.info("getting outputs for %s", env.workdir)
        env.move_output_data()
        env.wait_for_data_mover()
        transaction.commit()  # start fresh
        transaction.begin()   # and sync state
        env.import_output(context, env, OUTPUTS)
        transaction.commit()  # does this help?
        return env
    except Exception as e:
        import traceback
        LOG.fatal('get_outputs: %s', traceback.format_exc())
        raise Exception(str(e), env)
    finally:
        # TODO: don't delete here because transaction might need to be
        #       replayed or commit transaction above?'
        env.cleanup()
        #LOG.info("getting outputs cleaned up %s", env.workdir)
        # cleanup fabric state
        #env.close()


def run_job(env):
    # TODO: result of previous job might be a zc.twist.Failure
    try:
        env.start_script()
        # the remote job might take a while to start let's retry a few times
        # in case check_script fails to detect running job
        retry = 3
        while True:
            # TODO: make this some sort of time out and do proper
            #       error checking (e.g. connection failures)
            ret = env.check_script()
            if ret is None:
                retry = 0  # we have found something no retry necessary anymore
                LOG.info("Remote job not yet finished.")
                sleep(10)
                continue
            if retry:
                # we haven't seen a sign of the job yet, let's retry
                retry -= 1
                continue
            #-1 .... something went wrong
            # any other number ... exit code of job
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
        # TODO: make sure to fetch any results available (esp. log file)
        import traceback
        LOG.fatal('run_job: %s', traceback.format_exc())
        raise Exception(str(e), env)
    finally:
        # cleanup fabric state
        env.cleanup()
        #env.close()


# TODO: use getDatasetMetadat from xmlrpc package. (remove
#       getbiolayermetadata in getExperimentInfo)
def getDatasetInfo(datasetitem):
    # TODO: filename might be None, and then this job fails miserably
    int_url = os.environ.get("INTERNAL_URL", INTERNAL_URL)
    dsfilename = datasetitem.file.filename
    if dsfilename is None:
        # use datasetitem id in case we don't have a filename
        dsfilename = datasetitem.id
    dsurl = '{}{}/@@download/file/{}'.format(
        int_url, '/'.join(datasetitem.getPhysicalPath()),
        datasetitem.file.filename)
    # check for ArchiveItems in the graph and add stuff
    return {
        'filename': dsfilename,
        'url': dsurl
    }


def job_run(context, env, script, params, OUTPUTS):
    endstate = 'Failed'
    try:
        status = local.getLiveAnnotation('bccvl.status')
        status['task'] = 'Transferring'
        local.setLiveAnnotation('bccvl.status', status)
        #Queued,Completed,Failed,Transfering,Running,Retrieving
        create_workenv(env, script, params)
        status['task'] = 'Running'
        local.setLiveAnnotation('bccvl.status', status)
        # local.setLiveAnnotation('bccvl.status', status, job=main_job)
        run_job(env)
        # status['task'] = newstate
        # local.setLiveAnnotation('bccvl.status', status, job=main_job)
        status['task'] = 'Retrieving'
        local.setLiveAnnotation('bccvl.status', status)
        get_outputs(context, env, OUTPUTS)
        endstate = 'Completed'
    except Exception as e:
        # TODO: set job status and return value
        raise e
    finally:
        status['task'] = 'Cleanup'
        local.setLiveAnnotation('bccvl.status', status)
        env.cleanup()
        env.cleanup_remote()
        env.close()
        status['task'] = endstate
        local.setLiveAnnotation('bccvl.status', status)


# TODO: this should be generic. there should be only one queue_job for
#       all types of jobs
def queue_job(result, jobid, env, script, params, OUTPUTS):
    try:
        async = getUtility(IAsyncService)
        queues = async.getQueues()

        env.jobid = jobid
        run_job = async.wrapJob((job_run, result,
                                 (env, script, params, OUTPUTS), {}))
        run_job.jobid = env.jobid
        run_job.quota_names = ('default', )
        run_job.annotations['bccvl.status'] = {
            'step': 0,
            'task': u'Queued'}
        queue = queues['']
        queue.quotas['default'].size = 4
        # agent size:
        for dispagent in queue.dispatchers.values():
            if 'main' in dispagent:
                dispagent['main'].size = 2
        return queue.put(run_job)

    except Exception as e:
        env.cleanup()
        env.cleanup_remote()
        raise e


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
        if src['type'] == 'scp':
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
        # TODO: filename might be None, and then this job fails miserably
        destname = '/'.join((self.inputdir, datasetitem.file.filename))
        src = {'type': 'blob',
               'file': datasetitem.file.open()}
        dest = {'host': 'compute', 'path': destname}
        self.move_data(type, src, dest, uuid=IUUID(datasetitem))

    def wait_for_data_mover(self):
        # all local copy no wait here
        pass
