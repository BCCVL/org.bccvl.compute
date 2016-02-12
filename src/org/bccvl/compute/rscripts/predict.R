
# bccvl.params$future$data ... list of file names with climate variables

# bccvl.env$inputdir ... folder with all input data
# bccvl.env$outputdir ... folder to store output files

# dismo:
#   bccvl.params$tails ... predict (tails)
# biomod:
#   bccvl.params$selected_models ... which models to use
#   bccvl.params$compress ... not used

# dismo places outputfiles in outputdir
# biomod places outputfiles in subfolders in outputdir

sdm.species = bccvl.params$species_distribution_models$species
sdm.model.file = bccvl.params$species_distribution_models$filename
projection.name = bccvl.params$projection_name
#geographic constraints
enviro.data.constraints = bccvl.params$projection_region

future.climate.dataset = lapply(bccvl.params$future_climate_datasets, function(x) x$filename)
future.climate.data.type = lapply(bccvl.params$future_climate_datasets, function(x) x$type)

projectdataset <- function(model.obj, futuredata, datatype, projection.name, species) {
    future.climate.scenario = bccvl.enviro.stack(futuredata, datatype, resamplingflag="lowest")
    # filter out unused layers from future.climate.scenario
    predictors <- bccvl.checkModelLayers(model.obj, future.climate.scenario)
    # geographically constrained modelling
    if (!is.null(enviro.data.constraints)) {
      constrainedResults = bccvl.sdm.geoconstrained(predictors, NULL, enviro.data.constraints);
      predictors <- constrainedResults$raster
    }
    
    # do projection
    if (inherits(model.obj, "DistModel")) {
        # dismo package
        opt.tails <- bccvl.params$tails
        opt.ext <- NULL
        model.proj <- predict(model.obj,
                              predictors,
                              tails=opt.tails,
                              ext=opt.ext)
        bccvl.saveModelProjection(model.proj, projection.name, species)
    } else if (inherits(model.obj, "gbm")) {
        # brt package)
        model.proj <- predict(predictors,
                              model.obj,
                              n.trees=model.obj$gbm.call$best.trees,
                              type="response")
        bccvl.saveModelProjection(model.proj, projection.name, species)
    } else if (inherits(model.obj, "BIOMOD.models.out")) {
        # expect additional model data in input folder.
        # for biomod to find it we'll have to change wd

        biomod.xy.new.env <- NULL
        biomod.selected.models <- bccvl.params$selected_models
        biomod.binary.meth <- NULL
        biomod.filtered.meth <- NULL
        biomod.compress <- NULL # bccvl.params$compress
        biomod.build.clamping.mask <- TRUE
        biomod.species.name <-  species
        opt.biomod.silent <- FALSE
        opt.biomod.do.stack <- TRUE
        opt.biomod.keep.in.memory <- TRUE
        opt.biomod.output.format <- NULL

        model.proj <- BIOMOD_Projection(modeling.output=model.obj,
                                        new.env=predictors,
                                        proj.name=projection.name,
                                        xy.new.env=biomod.xy.new.env,
                                        selected.models=biomod.selected.models,
                                        binary.meth=biomod.binary.meth,
                                        filtered.meth=biomod.filtered.meth,
                                        # compress=biomod.compress, # .. Null not accepted
                                        build.clamping.mask=biomod.build.clamping.mask,
                                        silent=opt.biomod.silent,
                                        do.stack=opt.biomod.do.stack,
                                        keep.in.memory=opt.biomod.keep.in.memory,
                                        output.format=opt.biomod.output.format)
        # save projection to output folder
        # move proj_folder
        projinput <- file.path(getwd(),
                               biomod.species.name,
                               paste("proj", projection.name, sep="_"))
        projoutput <- file.path(bccvl.env$outputdir,
                                biomod.species.name,
                                paste("proj", projection.name, sep="_"))
        # create top level dir
        dir.create(file.path(bccvl.env$outputdir, biomod.species.name))
        # move proj_future folder to output folder
        file.rename(projinput, projoutput)
        # convert grd files to tif
        bccvl.grdtogtiff(projoutput)
    }
}


# TODO: get rid of this zip detection
# in case the sdm model is a zip file we'll have to unpack it
modelfile <- sdm.model.file
if (tolower(file_ext(modelfile)) == "zip") {
    # assume we have been given a zip file and the model file has the same name
    # as the basename(zip) in the same folder as the zip
    modeldir = dirname(modelfile)
    modelfile = file_path_sans_ext(basename(modelfile))
    modelfile = file.path(modeldir, modelfile)
    # we'll have to change wd so that biomod can load data from model subfolders'
    setwd(modeldir)
    # TODO: could make the part with getModelOjebct more intelligent...
    #       e.g. check if loaded model is a biomod model and change wd
}


# load model
# TODO:should be loaded straigt from bccvl.params$sdms[1]
model.obj <- bccvl.getModelObject(modelfile)

# use folder name of first dataset to generate name for projection output
projectdataset(model.obj, future.climate.dataset, future.climate.data.type, projection.name, sdm.species)
