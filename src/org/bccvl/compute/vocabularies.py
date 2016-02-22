"""Base classes for parameters of algorithms with biomod functionality"""

from zope.interface import provider
from zope.schema.vocabulary import SimpleVocabulary, SimpleTerm
from zope.schema.interfaces import IVocabularyFactory


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


lm_na_action_vocab = SimpleVocabulary([
    SimpleTerm('na.fail', 'na.fail', 'na.fail'),
    SimpleTerm('na.omit', 'na.omit', 'na.omit'),
    SimpleTerm('na.exclude', 'na.exclude', 'na.exclude'),
    SimpleTerm(None, 'NULL', 'NULL')
])


@provider(IVocabularyFactory)
def lm_na_action_vocab_factory(context):
    return lm_na_action_vocab


pa_strategy_vocab = SimpleVocabulary([
    SimpleTerm('random', 'random', 'random'),
    SimpleTerm('sre', 'sre', 'sre'),
    SimpleTerm('disk', 'disk', 'disk'),
    SimpleTerm('none', 'none', 'none'),
])


@provider(IVocabularyFactory)
def pa_strategy_vocab_factory(context):
    return pa_strategy_vocab
