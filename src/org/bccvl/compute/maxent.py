from pkg_resources import resource_string
from org.bccvl.compute.utils import WorkEnv, queue_job

from zope.interface import moduleProvides
from .rscript import execute_sdm
from .biomod import BIOMOD_OUTPUTS


OUTPUTS = BIOMOD_OUTPUTS


def execute(experiment, func, request=None):
    return execute_sdm(experiment, func, request, OUTPUTS=OUTPUTS)
