from pkg_resources import resource_string
import re
from org.bccvl.compute.utils import getdatasetparams
from gu.z3cform.rdf.interfaces import IResource
from org.bccvl.site.namespace import BIOCLIM, DWC
from plone.app.uuid.utils import uuidToCatalogBrain
# do this dynamically in site module?
from zope.interface import provider
from org.bccvl.site.interfaces import IComputeMethod
from copy import deepcopy
from plone import api
import tempfile
from org.bccvl.tasks.compute import r_task
from org.bccvl.tasks.plone import after_commit_task


def get_project_params(result):
    params = deepcopy(result.job_params)
    # get metadata for species_distribution_models
    uuid = params['species_distribution_models']
    params['species_distribution_models'] = getdatasetparams(uuid)
    # do biomod name mangling of species name
    params['species_distribution_models']['species'] = re.sub(u"[ _]", u".", params['species_distribution_models'].get('species', u"Unknown"))
    # we need the layers from sdm to fetch correct files for climate_models
    # TODO: getdatasetparams should fetch 'layers'
    sdmobj = uuidToCatalogBrain(uuid)
    sdmmd = IResource(sdmobj)
    layers = [l.identifier for l in sdmmd.objects(BIOCLIM['bioclimVariable'])]
    params['species_distribution_models']['layers'] = layers
    # do future climate layers
    uuid = params['future_climate_datasets']
    dsinfo = getdatasetparams(uuid)
    climatelist = []
    for layer in layers:
        climatelist.append({
            'uuid': dsinfo['uuid'],
            'filename': dsinfo['filename'],
            'downloadurl': dsinfo['downloadurl'],
            'internalurl': dsinfo['internalurl'],
            'layer': layer,
            'zippath': dsinfo['layers'][layer],
            # TODO: add year, gcm, emsc here?
            # TODO: do we have/need continuous or not?
            'type': dsinfo['type'],
        })
    # replace climate_models parameter
    params['future_climate_datasets'] = climatelist
    params['selected_models'] = 'all'
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
# FIXME: remove clapmingMasks from DataGenreFP
#        e.g. add exclude pattern and interpret as glob(includes) - glob(excludes)
#        or apply regexp pattern on glob(includes) result
OUTPUTS = {
    'files': {
        '*.Rout': {
            "title": "Log file",
            "genre": "DataGenreLog",
            "mimetype": "text/x-r-transcript"
        },
        "*ClampingMask.tif": {
            "title": "Clamping Mask",
            "genre": "DataGenreClampingMask",
            "mimetype": "image/geotiff"
        },
        'proj_*.tif': {
            'title': 'Future Projection',
            'genre': 'DataGenreFP',
            'mimetype': 'image/geotiff',
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
