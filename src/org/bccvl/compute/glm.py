from .rscript import execute_sdm
from .biomod import BIOMOD_OUTPUTS


OUTPUTS = BIOMOD_OUTPUTS


def execute(experiment, func, request=None):
    return execute_sdm(experiment, func, request, OUTPUTS=OUTPUTS)
