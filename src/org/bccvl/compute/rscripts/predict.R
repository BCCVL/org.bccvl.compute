
# bccvl.params$future$data ... list of file names with climate variables
# bccvl.params$inputmodel ... sdm model
# bccvl.params$inputdir ... folder with all input data
# bccvl.params$outputdir ... folder to store output files

# dismo:
#   bccvl.params$tails ... predict (tails)
# biomod:
#   bccvl.params$species ... used to generate output files and folders
#   bccvl.params$selected_models ... which models to use
#   bccvl.params$compress ... not used

# dismo places outputfiles in outputdir
# biomod places outputfiles in subfolders in outputdir

projection.name <- "future"

future.climate.scenario <- stack(bccvl.params$future$data)

# load model
model.obj <- bccvl.getModelObject(bccvl.params$inputmodel)

# filter out unused layers from future.climate.scenario
predictors <- bccvl.checkModelLayers(model.obj, future.climate.scenario)

# do projection
if (inherits(model.obj, "DistModel")) {
    # dismo package
    opt.tails <- bccvl.params$tails
    opt.ext <- NULL
    model.proj <- predict(model.obj,
                          predictors,
                          tails=opt.tails,
                          ext=opt.ext)
    bccvl.saveModelProjection(model.proj, projection.name)
} else if (inherits(model.obj, "gbm")) {
    # brt package)
    model.proj <- predict(predictors,
                          model.obj,
                          n.trees=model.obj$gbm.call$best.trees,
                          type="response")
    bccvl.saveModelProjection(model.proj, projection.name)
} else if (inherits(model.obj, "BIOMOD.models.out")) {
    # expect additional model data in input folder.
    # for biomod to find it we'll have to change wd
    setwd(bccvl.params$inputdir) # TODO: get foldername from bccvl.params$inputmodel

    biomod.xy.new.env <- NULL
    biomod.selected.models <- bccvl.params$selected_models
    biomod.binary.meth <- NULL
    biomod.filtered.meth <- NULL
    biomod.compress <- bccvl.params$compress
    biomod.build.clamping.mask <- TRUE
    biomod.species.name <- bccvl.params$species
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
    projoutput <- file.path(bccvl.params$outputdir,
                            biomod.species.name,
                            paste("proj", projection.name, sep="_"))
    # create top level dir
    dir.create(file.path(bccvl.params$outputdir, biomod.species.name))
    # move proj_future folder to output folder
    file.rename(projinput, projoutput)
    # convert grd files to tif
    bccvl.grdtogtiff(projoutput)
}
