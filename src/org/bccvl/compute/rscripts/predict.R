
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
sdm.projections.files = lapply(bccvl.params$sdm_projections, function(x) x$filename)
projection.name = bccvl.params$projection_name
projection.threshold = ifelse(is.null(bccvl.params$threshold), 0.5, bccvl.params$threshold)
#geographic constraints
enviro.data.constraints = bccvl.params$projection_region
#Indicate to generate and apply convex-hull polygon of occurrence dataset to constraint
enviro.data.generateCHall = ifelse(is.null(bccvl.params$generate_convexhull), FALSE, as.logical(bccvl.params$generate_convexhull))
# resampling (up / down scaling) if scale_down is TRUE, return 'lowest'
enviro.data.resampling = ifelse(is.null(bccvl.params$scale_down) ||
                                ! as.logical(bccvl.params$scale_down),
                                'highest', 'lowest')

future.climate.dataset = lapply(bccvl.params$future_climate_datasets, function(x) x$filename)
future.climate.data.type = lapply(bccvl.params$future_climate_datasets, function(x) x$type)
#layer names for the current environmental layers used
future.climate.data.layer = lapply(bccvl.params$future_climate_datasets, function(x) x$layer)

# constraint_type: null for protjection with constraint, and  "unconstraint" for projection without constraint.
projectdataset <- function(model.obj, futuredata, datatype, datalayername, projection.name, species, constraint_geojson, constraint_type=NULL) {
    future.climate.scenario = bccvl.enviro.stack(futuredata, datatype, datalayername, resamplingflag=enviro.data.resampling)
    # filter out unused layers from future.climate.scenario
    predictors <- bccvl.checkModelLayers(model.obj, future.climate.scenario, futuredata)
    # geographically constrained modelling
    if (!is.null(constraint_geojson) && is.null(constraint_type)) {
      # TODO: we have to be careful here, as the raster package may create a lot of temporary data here when we constrain input data, and run a projection
      # actual problem: ... the mask call in geoconstrained generates a whole lot of temporary gri/grd files. these are then fed into biomod (only maxent?). when biomod does it's thing, it again generates a lot of temporary raster files, which quickly fills up the disk  
      # solution: https://r-forge.r-project.org/forum/forum.php?max_rows=25&style=nested&offset=12&forum_id=995&group_id=302 ... split up predictors into small areas ... project each one... combine the mosaic result at the end... might even speed up projection as it is less disc intense
      constrainedResults = bccvl.sdm.geoconstrained(predictors, NULL, constraint_geojson, enviro.data.generateCHall);
      predictors <- constrainedResults$raster
        
    }

    # do projection
    tiffilepath <- NULL
    outdir <- bccvl.env$outputdir
    if (inherits(model.obj, "DistModel")) {
        # dismo package
        opt.tails <- bccvl.params$tails
        opt.ext <- NULL
        model.proj <- predict(model.obj,
                              predictors,
                              tails=opt.tails,
                              ext=opt.ext)
        tiffilepath <- bccvl.saveModelProjection(model.proj, projection.name, species, filename_ext=constraint_type)
    } else if (inherits(model.obj, "gbm")) {
        # brt package)
        model.proj <- predict(predictors,
                              model.obj,
                              n.trees=model.obj$gbm.call$best.trees,
                              type="response")
        tiffilepath <- bccvl.saveModelProjection(model.proj, projection.name, species, filename_ext=constraint_type)
    } else if (inherits(model.obj, "BIOMOD.models.out")) {
        # For biomod we process the raster in blocks to avoid creating an unnecessary large number of temporary raster files.

        projections = c()
        clampings = c()
        
        blocksize = 512

        # log progress
        steps = ceiling(ncol(predictors) / blocksize) * ceiling(nrow(predictors) / blocksize)
        step = 1
        
        for (c1 in seq(1, ncol(predictors), blocksize)) {
            for(r1 in seq(1, nrow(predictors), blocksize)) {
                c2 = min(c1 + blocksize - 1, ncol(predictors))
                r2 = min(r1 + blocksize - 1, nrow(predictors))
                block = stack(getValuesBlock_enhanced(predictors, r1, r2, c1, c2, format='raster'))
                block_name = paste(r1, c1, sep="_")

                # expect additional model data in input folder.
                # for biomod to find it we'll have to change wd

                biomod.xy.new.env <- NULL
                biomod.selected.models <- bccvl.params$selected_models
                biomod.binary.meth <- NULL
                biomod.filtered.meth <- NULL
                biomod.compress <- NULL # bccvl.params$compress
                biomod.build.clamping.mask <- FALSE
                biomod.species.name <-  species
                opt.biomod.silent <- TRUE
                opt.biomod.do.stack <- TRUE
                opt.biomod.keep.in.memory <- TRUE
                opt.biomod.output.format <- NULL

                cat(paste("\n", "Projecting block:", step, "of", steps, sep=" "))
                
                model.proj <- BIOMOD_Projection(modeling.output=model.obj,
                                                new.env=block,
                                                proj.name=block_name,
                                                xy.new.env=biomod.xy.new.env,
                                                selected.models=biomod.selected.models,
                                                binary.meth=biomod.binary.meth,
                                                filtered.meth=biomod.filtered.meth,
                                                # compress=biomod.compress, # .. Null not accepted
                                                build.clamping.mask=biomod.build.clamping.mask,
                                                silent=opt.biomod.silent,
                                                do.stack=opt.biomod.do.stack,
                                                keep.in.memory=opt.biomod.keep.in.memory,
                                                output.format=opt.biomod.output.format,
                                                on_0_1000=FALSE)

                step = step + 1
                
                # store paths
                # output raster model.proj@proj
                proj_folder = file.path(getwd(),
                                        model.obj@sp.name,
                                        paste("proj_", block_name, sep= ""))
                # convert projection output and clamping mask to geotiff
                # Set the nodatavalue explicitly to fix an issue with unrecognised default nodatavalue with gdal.
                # Shall be removed when gdal bug is fixed in gdal 2.1.3.
                bccvl.grdtogtiff(proj_folder, noDataValue=-4294967296)

                # collect geotiff file names for 
                projections = c(projections,
                                file.path(proj_folder,
                                          paste("proj_", block_name, "_", model.obj@sp.name, ".tif", sep="")))
                clampings = c(clampings,
                              file.path(proj_folder,
                                        paste("proj_", block_name,"_ClampingMask", ".tif", sep="")))
                                
                # free up temp space used up by biomod and raster
                removeTmpFiles(0)
            }
        }

        # create output folder
        outdir = file.path(bccvl.env$outputdir,
                           biomod.species.name,
                           paste("proj", projection.name, sep="_"))
        if (!dir.exists(outdir)) {
            dir.create(outdir, recursive=TRUE)
        }
        # combine all seperate rasters into one big file
        filename_ext = ifelse(is.null(constraint_type), "", paste("_", constraint_type, sep=""))
        if (biomod.build.clamping.mask) {
            mosaic_rasters(clampings,
                           file.path(outdir,
                                     paste("proj_", projection.name, "_ClampingMask", filename_ext, ".tif", sep="")),
                           co=c("COMPRESS=LZW", "TILED=YES"),
                           format="GTiff")
        }

        tiffilepath = file.path(outdir,
                                 paste("proj_", projection.name, "_", biomod.species.name, filename_ext, ".tif", sep=""))
        mosaic_rasters(projections, 
                       tiffilepath,
                       co=c("COMPRESS=LZW", "TILED=YES"),
                       format="GTiff")

        # save projection as png as well
        bccvl.saveProjectionImage(tiffilepath, projection.name, biomod.species.name, outputdir=outdir, filename_ext=constraint_type)
    }

    # Compute metrics only for unconstraint projection.
    if (length(sdm.projections.files) > 0 && !is.null(constraint_type)) {
        # get the correct sdm projection file
        sdm_projection_file = sdm.projections.files[[1]]

        # Make sure both projections have the same extent and resolution; scale it to the resolution of CC 
        data_types = list("continuous", "continuous")
        filenames = list(tiffilepath, sdm_projection_file)
        resamplingflag = ifelse(res(raster(tiffilepath))[1] >= res(raster(sdm_projection_file))[1], 'highest', 'lowest')
        proj_rasters = bccvl.rasters.to.common.extent.and.resampled.resolution(filenames, data_types, resamplingflag)

        # generate occurrence probability change for future and current projections
        changefilepath = bccvl.get_filepath("prob_change_", 
                                            projection.name, 
                                            species, 
                                            outputdir=outdir, 
                                            filename_ext=constraint_type,
                                            file_ext="tif")
        bccvl.generateOccurrenceProbChangeMetric(proj_rasters, changefilepath)

        # generate species range change metric and summary
        changefilepath = bccvl.get_filepath("range_change_", 
                                            projection.name, 
                                            species, 
                                            outputdir=outdir, 
                                            filename_ext=constraint_type,
                                            file_ext="tif")
        bccvl.generateSpeciesRangeChangeMetric(proj_rasters, projection.threshold, changefilepath)

        # Generate the Centre of Species Range metric
        changefilepath = bccvl.get_filepath("centre_species_range_", 
                                            projection.name, 
                                            species, 
                                            outputdir=outdir, 
                                            filename_ext=constraint_type,
                                            file_ext="csv")
        bccvl.generateCentreOfGravityMetric(filenames, changefilepath)
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
projectdataset(model.obj, future.climate.dataset, future.climate.data.type, future.climate.data.layer, projection.name, sdm.species, enviro.data.constraints)

# Do projection without any constraint only if there is constraint.
if (!is.null(enviro.data.constraints)) {
    projectdataset(model.obj, future.climate.dataset, future.climate.data.type, future.climate.data.layer, projection.name, sdm.species, enviro.data.constraints, constraint_type="unconstraint")
}
