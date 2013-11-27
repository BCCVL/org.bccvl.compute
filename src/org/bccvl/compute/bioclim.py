from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv, WorkEnvLocal, queue_job

from zope.interface import moduleProvides, implementer, Interface
# do this dynamically in site module?
from z3c.form.object import registerFactoryAdapter
from .interfaces import IComputeFunction

moduleProvides(IComputeFunction)


def get_bioclim_params(experiment):
    return {}


def generate_sdm_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/eval.R'),
        resource_string('org.bccvl.compute', 'rscripts/bioclim.R'),
    ])
    return script

OUTPUTS = {
    'files': {
        'AUC.png': {
            'title': 'AUC.png',
            'type': '???'},
        '*_response.png': {
            'title': '',
            'type': '???'},
        '*.csv': {
            'title': '',
            'type': 'csv', },
        'dismo.eval.object.RData': {
            'title': '',
            'type': 'RData', },
        'model.object.RData': {
            'title': '',
            'type': 'RData', },
        'results.html': {
            'title': '',
            'type': 'html', },
        'sdm.Rout': {
            'title': '',
            'type': 'html'},
        'current.tif': {
            'title': '',
        },
    },
    'archives': {
        'results.html.zip': {
            'files': ['results.html', 'AUC.png'],
            'type': 'report'},
    },
}


def execute(experiment, workenv=WorkEnv):
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
    # TODO: CREATE WorkEnv in job
    workenv = WorkEnvLocal
    env = workenv('localhost')
    params = get_bioclim_params(experiment)
    script = generate_sdm_script()
    return queue_job(experiment, 'Bioclim', env, script, params)


class IParametersBioclim(Interface):

    """there are no user-configurable options"""


@implementer(IParametersBioclim)
class ParametersBioclim(object):
    pass

registerFactoryAdapter(IParametersBioclim, ParametersBioclim)

parameters = IParametersBioclim

if __name__ == '__main__':
    execute()
