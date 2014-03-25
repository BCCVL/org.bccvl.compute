from .rscript import execute_sdm
from .bioclim import DISMO_OUTPUTS


OUTPUTS = DISMO_OUTPUTS


def execute(experiment, func, request=None):
    return execute_sdm(experiment, func, request, OUTPUTS=OUTPUTS)
