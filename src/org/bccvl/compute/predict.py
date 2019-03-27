from copy import deepcopy
from decimal import Decimal
import os.path
from pkg_resources import resource_string
import re

from plone import api
from plone.uuid.interfaces import IUUID
from plone.app.uuid.utils import uuidToObject
# do this dynamically in site module?
from zope.interface import provider
from zope.component.hooks import getSite

from org.bccvl.compute.utils import getdatasetparams
from org.bccvl.site.interfaces import IComputeMethod, IBCCVLMetadata
from org.bccvl.site.utils import get_results_dir
from org.bccvl.tasks.compute import r_task
from org.bccvl.tasks.plone import after_commit_task, HIGH_PRIORITY


def get_project_params(result):
    params = deepcopy(result.job_params)
    # get metadata for species_distribution_models
    uuid = params['species_distribution_models']
    params['species_distribution_models'] = getdatasetparams(uuid)
    # do biomod name mangling of species name
    params['species_distribution_models']['species'] = re.sub(u"[ _\-'\"/\(\)\{\}\[\]]", u".", params['species_distribution_models'].get('species', u"Unknown"))
    # we need the layers from sdm to fetch correct files for climate_models
    # TODO: getdatasetparams should fetch 'layers'
    sdmobj = uuidToObject(uuid)
    sdmmd = IBCCVLMetadata(sdmobj)
    params['species_distribution_models']['layers'] = sdmmd.get('layers_used', None)

    # do SDM projection results
    sdm_projections = []
    for resuuid in params['sdm_projections']:
         sdm_projections.append(getdatasetparams(resuuid))
    params['sdm_projections'] = sdm_projections

    # do future climate layers
    climatelist = []
    for uuid, layers in params['future_climate_datasets'].items():
        dsinfo = getdatasetparams(uuid)
        for layer in layers:
            dsdata = {
                'uuid': dsinfo['uuid'],
                'filename': dsinfo['filename'],
                'downloadurl': dsinfo['downloadurl'],
                'layer': layer,
                # TODO: add year, gcm, emsc here?
                'type': dsinfo['layers'][layer]['datatype'],
            }
            # if this is a zip file we'll have to set zippath as well
            # FIXME: poor check whether this is a zip file
            if dsinfo['filename'].endswith('.zip'):
                dsdata['zippath'] = dsinfo['layers'][layer]['filename']

            # FIXME: workaround to get future projection name back, but this works only for file naming scheme with current available data
            if params['selected_future_layers'] and layer in params['selected_future_layers']:
                params['projection_name'], _ = os.path.splitext(dsinfo['filename'])
            climatelist.append(dsdata)
    # replace climate_models parameter
    params['future_climate_datasets'] = climatelist
    params['selected_models'] = 'all'

    # In case no future climate layer is selected
    if not params.get('projection_name'):
        params['projection_name'], _ = os.path.splitext(dsinfo['filename'])

    # TODO: quick fix Decimal json encoding through celery (where is my custom json encoder gone?)
    for key, item in params.items():
        if isinstance(item, Decimal):
            params[key] = float(item)

    # Get the content of the projection_region BlobFile.
    # Note: deepcopy does not copy the content of BlobFile.
    params['projection_region'] = { 
            'uuid': IUUID(result),
            'filename': 'projection_region.json',
            'downloadurl': '{0}/API/em/v1/constraintregion?uuid={1}'.format(getSite().absolute_url(), IUUID(result)),
    }

    # add hints for worker
    workerhints = {
        'files': ('species_distribution_models', 'future_climate_datasets', 'sdm_projections', 'projection_region',)
    }
    return {'env': {}, 'params': params, 'worker': workerhints}


def generate_project_script():
    script = '\n'.join([
        resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
        resource_string('org.bccvl.compute', 'rscripts/predict.R'),
    ])
    return script

# TODO: maybe allow tal expressions or regexp match parameters to create more meaningful titles?
# FIXME: which projection get's which metadata? (GCM, emsc, scale, year)
def get_output(algorithm):
    if algorithm == 'maxent':
        layer = 'projection_suitablity'
        dtype = 'Continuous'
    elif algorithm in ('circles', 'convhull', 'voronoihull'):
        layer = 'projection_binary'
        dtype = 'Discrete'
    else:
        layer = 'projection_probability'
        dtype = 'Continuous'

    return {
        'files': {
            "Rplots.pdf": {
                "skip": True
            },
            # Dismo projection output
            'proj_*.tif': {
                'title': 'Future Projection map',
                'genre': 'DataGenreFP',
                'layer': layer,
                'data_type': dtype,
                'mimetype': 'image/geotiff',
                'order': 1
            },
            '*/proj_*/proj_*.tif': {
                "title": 'Future Projection map',
                "genre": 'DataGenreFP',
                'layer': layer,
                'data_type': dtype,
                "mimetype": 'image/geotiff',
                "order": 1
            },
            'proj_*_unconstrained.tif': {
                'title': 'Future Projection map',
                'genre': 'DataGenreFP_ENVLOP',
                'layer': layer,
                'data_type': dtype,
                'mimetype': 'image/geotiff',
                'order': 2
            },
            '*/proj_*/proj_*_unconstrained.tif': {
                "title": "Future Projection map",
                "genre": "DataGenreFP_ENVLOP",
                'layer': layer,
                'data_type': dtype,
                "mimetype": "image/geotiff",
                "order": 2
            },
            'proj_*.png': {
                'title': 'Future Projection graph',
                'genre': 'DataGenreFP_GRAPH',
                'mimetype': 'image/png',
                'order': 3
            },
            '*/proj_*/proj_*.png': {
                'title': 'Future Projection graph',
                'genre': 'DataGenreFP_GRAPH',
                'mimetype': 'image/png',
                'order': 3
            },
            # Biomod projection output
            '*/proj_*/proj_*_ClampingMask.tif': {
                "title": "Clamping Mask",
                "genre": "DataGenreClampingMask",
                "mimetype": "image/geotiff",
                "layer": "clamping_mask",
                "data_type": "Discrete",
                "order": 4
            },
            'prob_change_*.tif': {
                'title': 'Change in probability map',
                'genre': 'DataGenreClimateChangeMetricMap',
                'layer': 'probability_difference',
                'data_type': 'Continuous',
                'mimetype': 'image/geotiff',
                "order": 5
            },
            '*/proj_*/prob_change_*.tif': {
                'title': 'Change in probability map',
                'genre': 'DataGenreClimateChangeMetricMap',
                'layer': 'probability_difference',
                'data_type': 'Continuous',
                'mimetype': 'image/geotiff',
                "order": 5
            },
            'range_change_*.tif': {
                'title': 'Change in species range map',
                'genre': 'DataGenreClimateChangeMetricMap',
                'layer': 'range_change',
                "data_type": "Discrete",
                'mimetype': 'image/geotiff',
                "order": 6
            },
            '*/proj_*/range_change_*.tif': {
                'title': 'Change in species range map',
                'genre': 'DataGenreClimateChangeMetricMap',
                'layer': 'range_change',
                "data_type": "Discrete",
                'mimetype': 'image/geotiff',
                "order": 6
            },
            'range_change_*.csv': {
                'title': 'Change in species range table',
                'genre': 'DataGenreClimateChangeMetric',
                'mimetype': 'text/csv',
                "order": 7
            },
            '*/proj_*/range_change_*.csv': {
                'title': 'Change in species range table',
                'genre': 'DataGenreClimateChangeMetric',
                'mimetype': 'text/csv',
                "order": 7
            },
            'centre_species_range_*.csv': {
                'title': 'Change in centre of species range table',
                'genre': 'DataGenreClimateChangeMetric',
                'mimetype': 'text/csv',
                "order": 8
            },
            '*/proj_*/centre_species_range_*.csv': {
                'title': 'Change in centre of species range table',
                'genre': 'DataGenreClimateChangeMetric',
                'mimetype': 'text/csv',
                "order": 8
            },
            '*.R': {
                'title': 'Job Script',
                'genre': 'JobScript',
                'mimetype': 'text/x-r',
                'order': 9
            },
            '*.Rout': {
                "title": "Log file",
                "genre": "DataGenreLog",
                "mimetype": "text/x-r-transcript",
                "order": 10
            },
            "params.json": {
                "title": "Input parameters",
                "genre": "InputParams",
                "mimetype": "text/x-r-transcript",
                "order": 100
            }
        },
        'archives': {
            # 'results.html.zip': {
            #     'files': ['results.html', 'AUC.png'],
            #     'title': 'Accuracy measures report as zip',
            #     'type': 'eval',
            #     'format': 'zip',
        },
    }


@provider(IComputeMethod)
def execute(result, func, priority=HIGH_PRIORITY):
    """
    This function takes an experiment and executes.

    It usesenvirnoment variables WORKER_DIR or HOME as root folder to execute
    experiments.

    After the execution finishes the output files will be attached to the
    experiment.

    :param experiment: The experiment holding the configuration and receiving
                       the results
    :type experiment: org.bccvl.site.content.IExperiment


    """
    params = get_project_params(result)
    script = generate_project_script()
    ### plone context for this job
    member = api.user.get_current()
    context = {
        'context': '/'.join(result.getPhysicalPath()),
        'user': {'id': member.getUserName(),
                 'email': member.getProperty('email'),
                 'fullname': member.getProperty('fullname')
                 },
        'experiment': {'title': result.__parent__.title,
                       'url': result.__parent__.absolute_url()
                       }
    }
    ### add result infos
    params['result'] = {
        'results_dir': get_results_dir(result, result.REQUEST),
        'outputs': get_output(result.job_params['function'])
    }
    params['worker']['script'] = {
        'name': 'projection.R',
        'script': script
    }
    # set debug flag
    params['worker']['zipworkenv'] = api.env.debug_mode()
    after_commit_task(r_task, priority, params, context)
