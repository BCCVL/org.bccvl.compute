from copy import deepcopy
from decimal import Decimal
import json
import logging
from pkg_resources import resource_string
import re

from plone import api
from zope.interface import provider

from org.bccvl.compute.utils import getdatasetparams
from org.bccvl.site.interfaces import IComputeMethod
from org.bccvl.site.utils import get_results_dir
from org.bccvl.tasks.compute import r_task
from org.bccvl.tasks.plone import after_commit_task


LOG = logging.getLogger(__name__)

# TODO: jobtracker: split job params accordingly and store on result (get's rid of result.ttolkit-> importer and possibly other places')
# TODO: adapt sdm scripts (bccvl.params to new param layout)
# TODO: adapt utils worker for new file management
# TODO: adapt getdatasetparams for different dataset with metadata
# TODO: adapt utils worker to populate 'env' section
# TODO: Tool could create import folder upfront so that worker can send results back immediately

# TODO: Job tracking alternative .... let object provide marker interface like IActiveJob, IFinishedJob, IFaildeJob etc... if on result it doesn't matter no job history needed (except for failed-> retry?)
#       woulde be kinda weird, ad iniefficient to access job info, but would do the trick


def get_sdm_params(result):
    # make a deep copy of the params to not accedientially modify the
    # persisted dict
    params = deepcopy(result.job_params)
    # TODO: names to fix up
    # occurrence-> species_occurrence_dataset
    # background-> species_absence_dataset
    # pseudoabsence['enabled']-> species_pseudo_absence_points,
    # pseudoabsence['points']-> species_number_pseudo_absence_points
    # layers+ environment{}-> environmental_datasets TODO: turn into list of files

    # get all necessary metadata for files, and add worker hints to download files
    for paramname in ('species_occurrence_dataset', 'species_absence_dataset'):
        # Skip empty or non existing params
        if not params.get(paramname, None):
            continue
        uuid = params[paramname]
        dsinfo = getdatasetparams(uuid)
        if dsinfo['filename'].endswith('.zip'):
            # FIXME: too many static assumptions about how an occurrence zip file looks like
            #        layers:key does not match anything (should it?)
            #        assumes exactly one file here
            # TODO: should I remove 'layers' section here?
            dsinfo['zippath'] = dsinfo['layers'].values()[0]['filename']
        params[paramname] =  dsinfo
        # replace all spaces and underscores to '.' (biomod does the same)
        # TODO: really necessary?
        if params[paramname]:
            params[paramname]['species'] = re.sub(u"[ _\-,'\"/\(\)\{\}\[\]]", u".", params[paramname].get('species', u'Unknown'))
    # TODO: This assumes we only zip file based layers
    envlist = []
    for uuid, layers in params['environmental_datasets'].items():
        dsinfo = getdatasetparams(uuid)
        for layer in layers:
            dsdata = {
                'uuid': dsinfo['uuid'],
                'filename': dsinfo['filename'],
                'downloadurl': dsinfo['downloadurl'],
                # TODO: should we use layer title or URI?
                'layer': layer,
                'type': dsinfo['layers'][layer]['datatype']
            }
            # if this is a zip file we'll have to set zippath as well
            # FIXME: poor check whether this is a zip file
            if dsinfo['filename'].endswith('.zip'):
                dsdata['zippath'] = dsinfo['layers'][layer]['filename']
            envlist.append(dsdata)
    # replace original dict
    params['environmental_datasets'] = envlist

    # TODO: quick fix Decimal json encoding through celery (where is my custom json encoder gone?)
    for key, item in params.items():
        if isinstance(item, Decimal):
            params[key] = float(item)

    # Get the content of the modelling_region BlobFile.
    # Note: deepcopy does not copy the content of BlobFile.
    if result.job_params['modelling_region']:
        params['modelling_region'] = result.job_params['modelling_region'].data

    # add hints for worker to download files
    workerhints = {
        # only those parameters that are actually in params dict
        'files':  [x for x in ('species_occurrence_dataset', 'species_absence_dataset', 'environmental_datasets') if x in params]
    }
    return {'env': {}, 'params': params, 'worker': workerhints}


def get_toolkit_params(result):
    # TODO: get params from result directly
    params = get_sdm_params(result)
    params['params'].update({
        # TODO: some params are probably sdm specific or even
        #       per run (in case of multi runs)
        #       following are mostly biomod specific but could be generic
        'rescale_all_models': False,
        'selected_models': 'all',
        'modeling_id': 'bccvl',
        # generic dismo params
        'tails': 'both',
        })
    return params


def generate_sdm_script(r_script):
    # TODO: clean this up ...
    #       e.g. generic sdm.R is entry point, which sources bccvl.R and eval.R
    #            and toolkit.R (source toolkit.R does the job)
    #       script name and source becomes part of params?
    #       workerhints may get script stuff as additional info?
    script = u'\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        r_script])
    return script


@provider(IComputeMethod)
def execute_sdm(result, toolkit):
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
    try:
        OUTPUTS = json.loads(toolkit.output)
    except (ValueError, TypeError) as e:
        LOG.fatal("couldn't load OUTPUT from toolkit %s: %s",
                  toolkit.getId(), e)
        OUTPUTS = {}
    params = get_toolkit_params(result)
    script = generate_sdm_script(toolkit.script)
    ###### generate plone context infos
    member = api.user.get_current()
    context = {
        'context': '/'.join(result.getPhysicalPath()),
        'user': {
            'id': member.getUserName(),
            'email': member.getProperty('email'),
            'fullname': member.getProperty('fullname')
        },
        'experiment': {
            'title': result.__parent__.title,
            'url': result.__parent__.absolute_url()
        }
    }
    ##### complete job infos
    params['result'] = {
        # FIXME: not optimal to access request this way
        #        rather pass in as parameter
        'results_dir': get_results_dir(result, result.REQUEST),
        'outputs': OUTPUTS
    }
    params['worker']['script'] = {
        'name': '{}.R'.format(toolkit.getId()),
        'script': script
    }
    # set debug flag
    params['worker']['zipworkenv'] = api.env.debug_mode()
    ### send job to queue

    # TODO: define job chain here (and in other methods as well)
    after_commit_task(r_task, params, context)
