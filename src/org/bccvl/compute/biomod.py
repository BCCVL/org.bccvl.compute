"""Base classes for parameters of algorithms with biomod functionality"""

from persistent import Persistent
from zope import schema
from zope.interface import Interface
from zope.schema.vocabulary import SimpleVocabulary
from zope.schema.fieldproperty import FieldProperty

from org.bccvl.compute import MessageFactory as _


biomod_prevalance_vocab = SimpleVocabulary.fromValues([
    'NULL', 0, 1
])

class IParametersBiomod(Interface):
    nb_run_eval = schema.Int(
        title=_(u'n-fold cross validation'),
        default=10,
    )
    data_split = schema.Int(
        title=_(u'data split'),
        default=100,
    )
#    y_weights = schema.Int(
#        title=_(u'response point weights'),
#        default=0, # TODO: should be 'NULL'? maybe do a vocab?
#    )
    prevalence = schema.Choice(
        title=_(u'weighted response weights'),
        vocabulary=biomod_prevalance_vocab,
        default='NULL',
    )
    var_import = schema.Int(
        title=_(u'resampling'),
        description=_(u'number of resampling of each explanatory variable to measure the relative importance of each variable for each selected model'),
        default=0,
    )
    rescale_all_models = schema.Bool(
        title=_(u'rescale all models'),
        description=_(u'scale all model prediction with a bionomial GLM?'),
        default=True,
    )
    do_full_models = schema.Bool(
        title=_(u'do full models'),
        description=_(u'calibrate & evaluate models with the whole dataset?'),
        default=True,
    )

field_property = lambda field_name: FieldProperty(IParametersBiomod[field_name])

class ParametersBiomod(Persistent):
    nb_run_eval = field_property('nb_run_eval')
    data_split = field_property('data_split')
#    y_weights = field_property('y_weights')
    prevalence = field_property('prevalence')
    var_import = field_property('var_import')
    rescale_all_models = field_property('rescale_all_models')
    do_full_models = field_property('do_full_models')


def get_biomod_params(param_object):
    """modify the params dict"""
    return {
        'nb_run_eval': param_object.nb_run_eval,
        'data_split': param_object.data_split,
        #'y_weights': param_object.y_weights,
        'prevalence': param_object.prevalence,
        'var_import': param_object.var_import,
        'rescale_all_models': param_object.rescale_all_models,
        'do_full_models': param_object.do_full_models
        }
