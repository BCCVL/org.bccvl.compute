from zope.interface import provider
from zope.schema.vocabulary import SimpleVocabulary
from zope.schema.interfaces import IVocabularyFactory
from .rscript import execute_sdm
from .biomod import BIOMOD_OUTPUTS


OUTPUTS = BIOMOD_OUTPUTS


def execute(experiment, func, request=None):
    return execute_sdm(experiment, func, request, OUTPUTS=OUTPUTS)

## Parameters
