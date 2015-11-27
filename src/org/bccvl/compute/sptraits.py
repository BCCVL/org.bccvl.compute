from copy import deepcopy
import json
import logging
from pkg_resources import resource_string

from plone import api
# do this dynamically in site module?
from zope.interface import provider

from org.bccvl.compute.utils import getdatasetparams
from org.bccvl.site.interfaces import IComputeMethod
from org.bccvl.site.utils import get_results_dir
from org.bccvl.tasks.compute import r_task
from org.bccvl.tasks.plone import after_commit_task


LOG = logging.getLogger(__name__)


def get_traits_params(result):
    params = deepcopy(result.job_params)
    # get metadata for species_distribution_models
    uuid = params['data_table']
    params['data_table'] = getdatasetparams(uuid)
    # add hints for worker
    workerhints = {
        'files': ('data_table', )
    }
    return {'env': {}, 'params': params, 'worker': workerhints}


def generate_traits_script(rscript):
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        rscript,
    ])
    return script


@provider(IComputeMethod)
def execute(result, toolkit):
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
    ### plone context for this job
    member = api.user.get_current()
    context = {
        'context': '/'.join(result.getPhysicalPath()),
        'user': {'id': member.getUserName(),
                 'email': member.getProperty('email'),
                 'fullname': member.getProperty('fullname')
                 },
        'experiment': {'title': result.__parent__.title,
                       'url': result.__parent__.absolute_url()
                       }
    }
    ### add result infos
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
    after_commit_task(r_task, params, context)
