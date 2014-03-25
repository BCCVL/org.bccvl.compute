from .rscript import execute_sdm

DISMO_OUTPUTS = {
    'files': {
        'AUC.png': {
            'title': 'Area Under the Receiver Operating'
                     ' Characteristic Curve (AUC)',
            'type': 'eval',
            'format': 'png',  # TODO: replace format withe mime/type?
        },
        '*.csv': {
            'title': 'Model accuracy statistics',
            'type': 'eval',
            'format': 'csv',
        },
        'dismo.eval.object.RData': {
            'title': 'R ModelEvaluation object',
            'type': 'eval',
            'format': 'RData',
        },
        'model.object.RData': {
            'title': 'R SDM Model object',
            'type': 'model',
            'format': 'RData',
        },
        'results.html': {
            'title': 'Accuracy measures report',
            'type': 'eval',
            'format': 'html',
        },
        '*.Rout': {
            'title': 'Log file',
            'type': 'log',
            'format': 'txt',
        },
        'current.tif': {
            'title': 'Projection to current',
            'type': 'projection',
            'format': 'GTiff',
        },
    },
    'archives': {
        'results.html.zip': {
            'files': ['results.html', 'AUC.png'],
            'title': 'Accuracy measures report as zip',
            'type': 'eval',
            'format': 'zip',
        },
    },
}

OUTPUTS = DISMO_OUTPUTS
OUTPUTS['files'].update({
    '*_response.png': {
        'title': 'Marginal Response Curve',
        'type': 'eval',
        'format': 'png',
    },
})


def execute(experiment, func, request=None):
    return execute_sdm(experiment, func, request, OUTPUTS=OUTPUTS)
