from pkg_resources import resource_string
import re
from org.bccvl.compute.utils import getdatasetparams
from plone.app.uuid.utils import uuidToObject
# do this dynamically in site module?
from zope.interface import provider
from org.bccvl.site.interfaces import IComputeMethod, IBCCVLMetadata
from copy import deepcopy
from plone import api
import tempfile
from org.bccvl.tasks.compute import r_task
from org.bccvl.tasks.plone import after_commit_task
import os.path


def get_project_params(result):
    params = deepcopy(result.job_params)
    # get metadata for species_distribution_models
    uuid = params['species_distribution_models']
    params['species_distribution_models'] = getdatasetparams(uuid)
    # do biomod name mangling of species name
    params['species_distribution_models']['species'] = re.sub(u"[ _]", u".", params['species_distribution_models'].get('species', u"Unknown"))
    # we need the layers from sdm to fetch correct files for climate_models
    # TODO: getdatasetparams should fetch 'layers'
    sdmobj = uuidToObject(uuid)
    sdmmd = IBCCVLMetadata(sdmobj)
    params['species_distribution_models']['layers'] = sdmmd.get('layers', None)
    # do future climate layers
    climatelist = []
    for uuid, layers in params['future_climate_datasets'].items():
        dsinfo = getdatasetparams(uuid)
        for layer in layers:
            dsdata = {
                'uuid': dsinfo['uuid'],
                'filename': dsinfo['filename'],
                'downloadurl': dsinfo['downloadurl'],
                'internalurl': dsinfo['internalurl'],
                'layer': layer,
                'zippath': dsinfo['layers'][layer]['filename'],
                # TODO: add year, gcm, emsc here?
                'type': dsinfo['layers'][layer]['datatype'],
            }
            # if this is a zip file we'll have to set zippath as well
            # FIXME: poor check whether this is a zip file
            if dsinfo['filename'].endswith('.zip'):
                dsdata['zippath'] = dsinfo['layers'][layer]['filename']
            climatelist.append(dsdata)
    # replace climate_models parameter
    params['future_climate_datasets'] = climatelist
    params['selected_models'] = 'all'
    # projection.name from dsinfo
    # FIXME: workaround to get future projection name back, but this works only for file naming scheme with current available data
    params['projection_name'], _ = os.path.splitext(dsinfo['filename'])
    # add hints for worker
    workerhints = {
        'files': ('species_distribution_models', 'future_climate_datasets')
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
OUTPUTS = {
    'files': {
        '*.Rout': {
            "title": "Log file",
            "genre": "DataGenreLog",
            "mimetype": "text/x-r-transcript"
        },
        # Dismo projection output
        'proj_*.tif': {
            'title': 'Future Projection',
            'genre': 'DataGenreFP',
            'mimetype': 'image/geotiff',
        },
        # Biomod projection output
        '*/proj_*/proj_*_ClampingMask.tif': {
            "title": "Clamping Mask",
            "genre": "DataGenreClampingMask",
            "mimetype": "image/geotiff"
        },
        "*/proj_*/proj_*.tif": {
            "title": "Future Projection",
            "genre": "DataGenreFP",
            "mimetype": "image/geotiff"
        },
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
def execute(result, func):
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
        'results_dir': tempfile.mkdtemp(),
        'outputs': OUTPUTS
    }
    params['worker']['script'] = {
        'name': 'projection.R',
        'script': script
    }
    # set debug flag
    params['worker']['zipworkenv'] = api.env.debug_mode()
    after_commit_task(r_task, params, context)
