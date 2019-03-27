from copy import deepcopy
from decimal import Decimal
import json
import logging
from pkg_resources import resource_string

from plone import api
from plone.uuid.interfaces import IUUID
# do this dynamically in site module?
from zope.interface import provider
from zope.component.hooks import getSite

from org.bccvl.compute.utils import getdatasetparams
from org.bccvl.site.interfaces import IComputeMethod
from org.bccvl.site.utils import get_results_dir
from org.bccvl.tasks.compute import r_task
from org.bccvl.tasks.plone import after_commit_task, HIGH_PRIORITY


LOG = logging.getLogger(__name__)


def get_traits_params(result):
    params = deepcopy(result.job_params)
    # get metadata for species_distribution_models
    for paramname in ('traits_dataset',):
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
        params[paramname] = dsinfo
    # TODO: This assumes we only zip file based layers
    envlist = []
    envds = params.get('environmental_datasets') or {}
    for uuid, layers in envds.items():
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

    # Get the content of the modelling_region BlobFile.
    # Note: deepcopy does not copy the content of BlobFile.
    if result.job_params['modelling_region']:
        params['modelling_region'] = { 
                'uuid': IUUID(result),
                'filename': 'modelling_region.json',
                'downloadurl': '{0}/API/em/v1/constraintregion?uuid={1}'.format(getSite().absolute_url(), IUUID(result)),
        }

    # add hints for worker
    workerhints = {
        'files': [x for x in ('traits_dataset', 'environmental_datasets', 'modelling_region',) if x in params]
    }
    return {'env': {}, 'params': params, 'worker': workerhints}


def generate_traits_script(rscript):
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/traits.R'),
        rscript,
    ])
    return script


@provider(IComputeMethod)
def execute(result, toolkit, priority=HIGH_PRIORITY):
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
    params = get_traits_params(result)
    script = generate_traits_script(toolkit.script)
    # plone context for this job
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

    # TODO: quick fix Decimal json encoding through celery (where is my custom
    # json encoder gone?)
    for key, item in params.items():
        if isinstance(item, Decimal):
            params[key] = float(item)

    # add result infos
    params['result'] = {
        'results_dir': get_results_dir(result, result.REQUEST),
        'outputs': OUTPUTS
    }
    params['worker']['script'] = {
        'name': '{}.R'.format(toolkit.getId()),
        'script': script
    }
    # set debug flag
    params['worker']['zipworkenv'] = api.env.debug_mode()
    after_commit_task(r_task, priority, params, context)
