from .rscript import execute_sdm
from zope.schema.vocabulary import SimpleVocabulary
from zope.interface import provider
from .biomod import BIOMOD_OUTPUTS
from zope.schema.interfaces import IVocabularyFactory


OUTPUTS = BIOMOD_OUTPUTS


def execute(experiment, func, request=None):
    return execute_sdm(experiment, func, request, OUTPUTS=OUTPUTS)

## parameters

glm_type_vocab = SimpleVocabulary.fromValues([
    'simple',
    'quadratic',
    'polynomial',
])


@provider(IVocabularyFactory)
def glm_type_vocab_factory(context):
    return glm_type_vocab


glm_test_vocab = SimpleVocabulary.fromValues([
    'AIC',
    'BIC',
    'none',
])


@provider(IVocabularyFactory)
def glm_test_vocab_factory(context):
    return glm_test_vocab


glm_family_vocab = SimpleVocabulary.fromValues([
    'binomial',
    'gaussian',
    'gamma',
    'inverse.gaussian',
    'poisson',
    'quasi',
    'quasibinomial',
    'quasipoisson',
])


@provider(IVocabularyFactory)
def glm_family_vocab_factory(context):
    return glm_family_vocab
