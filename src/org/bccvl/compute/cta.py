from zope.schema.interfaces import IVocabularyFactory
from zope.schema.vocabulary import SimpleVocabulary
from zope.interface import provider
from .rscripm import execute_sdm
from .biomod import BIOMOD_OUTPUTS


OUTPUTS = BIOMOD_OUTPUTS


def execute(experiment, func, request=None):
    return execute_sdm(experiment, func, request, OUTPUTS=OUTPUTS)

## Parameters

cta_method_vocab = SimpleVocabulary.fromValues([
    'anova',
    'class',
    'exp',
    'poisson',
])


@provider(IVocabularyFactory)
def cta_method_vocab_factory(context):
    return cta_method_vocab
