from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv
from plone.app.uuid.utils import uuidToObject

from zope.interface import moduleProvides, implementer, Interface
from z3c.form.object import registerFactoryAdapter # do this dynamically in site module?
from .interfaces import IComputeFunction

moduleProvides(IComputeFunction)


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
            'type': 'GEOTiff',
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
    occurrence = uuidToObject(experiment.species_occurrence_dataset)
    absence = uuidToObject(experiment.species_absence_dataset)
    climate = uuidToObject(experiment.environmental_dataset)
    # TODO: WORKER_DIR is gone
    try:
        from org.bccvl.compute.utils import WorkEnvLocal
        workenv = WorkEnvLocal
        env = workenv('localhost')
        env.prepare_work_env(climate, occurrence, absence)
        params = env.get_sdm_params()
        script = generate_sdm_script()
        env.execute(script, params)
        # FIXME: import should run in an async callback.
        env.import_output(experiment)
    finally:
        env.cleanup()


class IParametersBioclim(Interface):
    """there are no user-configurable options"""


@implementer(IParametersBioclim)
class ParametersBioclim(object):
    pass

registerFactoryAdapter(IParametersBioclim, ParametersBioclim)

parameters = IParametersBioclim

if __name__ == '__main__':
    execute()
