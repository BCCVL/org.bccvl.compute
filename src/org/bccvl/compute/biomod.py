"""Base classes for parameters of algorithms with biomod functionality"""

from persistent import Persistent
from zope import schema
from zope.interface import Interface, provider
from zope.schema.vocabulary import SimpleVocabulary
from zope.schema.fieldproperty import FieldProperty
from zope.schema.interfaces import IVocabularyFactory

from org.bccvl.compute import MessageFactory as _

BIOMOD_OUTPUTS = {
    'files': {
        '*.Rout': {
            'title': 'Log file',
            'type': 'log',
            'format': 'txt',
        },
        '*.txt': {
            'title': 'Model Evaluation',
            'type': 'eval',
            'format': 'txt',
        },
        'species/proj_current/proj_current_ClampingMask.tif': {
            'title': 'Clamping Mask',
            'type': 'projection',
            'format': 'GTiff',
        },
        'species/proj_current/proj_current_species.tif': {
            'title': 'Projection to current',
            'type': 'projection',
            'format': 'GTiff',
        },
    },
    'archives': {
        'model.object.RData.zip': {
            'files': ['model.object.RData',
                      'species/.BIOMOD_DATA/bccvl/*',
                      'species/models/bccvl/*',
                      ],
            'title': 'R SDM Model object',
            'type': 'model',
            'format': 'zip',
        },
    },
}


biomod_prevalance_vocab = SimpleVocabulary.fromValues([
    None, 0, 1
])


@provider(IVocabularyFactory)
def biomod_prevalance_vocab_factory(context):
    return biomod_prevalance_vocab


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
        vocabulary='biomod_prevalance_vocab',
        missing_value = '',
        default=None,
    )
    var_import = schema.Int(
        title=_(u'resampling'),
        description=_(u'number of resampling of each explanatory variable to measure the relative importance of each variable for each selected model'),
        default=0,
    )
    # rescale_all_models = schema.Bool(
    #     title=_(u'rescale all models'),
    #     description=_(u'scale all model prediction with a bionomial GLM?'),
    #     default=False,
    # )
    do_full_models = schema.Bool(
        title=_(u'do full models'),
        description=_(u'calibrate & evaluate models with the whole dataset?'),
        default=True,
    )


class ParametersBiomod(Persistent):
    nb_run_eval = FieldProperty(IParametersBiomod['nb_run_eval'])
    data_split = FieldProperty(IParametersBiomod['data_split'])
    # y_weights = FieldProperty(IParametersBiomod['y_weights'])
    prevalence = FieldProperty(IParametersBiomod['prevalence'])
    var_import = FieldProperty(IParametersBiomod['var_import'])
    #rescale_all_models = FieldProperty(IParametersBiomod['rescale_all_models'])
    do_full_models = FieldProperty(IParametersBiomod['do_full_models'])


def get_biomod_params(param_object):
    """modify the params dict"""
    return {
        'nb_run_eval': param_object.nb_run_eval,
        'data_split': param_object.data_split,
        #'y_weights': param_object.y_weights,
        'prevalence': param_object.prevalence,
        'var_import': param_object.var_import,
        'rescale_all_models': False,  # param_object.rescale_all_models,
        'do_full_models': param_object.do_full_models,
        'selected_models': 'all',
        'modeling_id': 'bccvl',
        'species': 'species',
        }
