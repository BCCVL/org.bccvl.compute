"""Base classes for parameters of algorithms with biomod functionality"""

from zope.interface import provider
from zope.schema.vocabulary import SimpleVocabulary
from zope.schema.interfaces import IVocabularyFactory

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
