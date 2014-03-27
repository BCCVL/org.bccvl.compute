from zope.schema.vocabulary import SimpleVocabulary, SimpleTerm
from zope.schema.interfaces import IVocabularyFactory
from zope.interface import provider

from .rscript import execute_sdm
from .bioclim import DISMO_OUTPUTS
OUTPUTS = DISMO_OUTPUTS


def execute(experiment, func, request=None):
    return execute_sdm(experiment, func, request, OUTPUTS=OUTPUTS)


## Parameters

brt_var_monotone_vocab = SimpleVocabulary([
    SimpleTerm(-1, '-1', u'-1'),
    SimpleTerm(1, '+1', u'+1'),
])


@provider(IVocabularyFactory)
def brt_var_monotone_vocab_factory(context):
    return brt_var_monotone_vocab

brt_family_vocab = SimpleVocabulary([
    SimpleTerm('bernoulli', 'bernoulli', 'bernoulli (binomial)'),
    SimpleTerm('poisson', 'poisson', 'poisson'),
    SimpleTerm('laplace', 'laplace', 'laplace'),
    SimpleTerm('gaussian', 'gaussian', 'gaussian'),
])


@provider(IVocabularyFactory)
def brt_family_vocab_factory(context):
    return brt_family_vocab
