###############
#
# predict(object, x, ext=NULL, filename="", progress='text', ...)
#
# object A fitted model of class Bioclim, Domain, MaxEnt, ConvexHull, or Mahalanobis (classes that inherit from DistModel)
# x A Raster* object or a data.frame
# ext An extent object to limit the prediction to a sub-region of 'x'. Or an object that can be coerced to an Extent object by extent; such as a Raster* or Spatial* object
# filename Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot
# progress Character. Valid values are "" (no progress bar), "text" and "windows" (on that platform only)
# ... Additional model specific arguments. And additional arguments for file writing as for writeRaster
#
# For maxent models, there is an additional argument 'args' used to pass arguments (options) to the maxent software.
# For bioclim models, there is an additional argument 'tails' which you can use to ignore the left or right tail of the percentile distribution for a variable.
# For geoDist models, there is an additional argument fun that allows you to use your own (inverse) distance function, and argument scale=1 that allows you to scale
# the values (distances smaller than this value become one, and the others are divided by this value before computing the inverse distance).
# For spatial predictions with BRT, randomForest, etc., see 'predict' in the Raster package
#
###############





###project the models onto FUTURE climate and save raster files
if (project.bioclim) {
    bioclim.obj = getModelObject("bioclim")	# get the model object
    if (!is.null(bioclim.obj)) {
        bioclim.proj = predict(bioclim.obj, future.climate.scenario, tails=opt.tails)	# predict for given climate scenario
        saveModelProjection(bioclim.proj, "bioclim", "future") # save output
    } else {
        write(paste("FAIL!", species, "Cannot load bioclim.obj from", wd, "output_bioclim", sep=": "), stdout())
    }
} # end if bioclim


if (project.brt) {
    brt.obj = getModelObject("brt") # get the model object
    if (!is.null(brt.obj)) {
        # NOTE the order of arguments in the predict function for brt; this is because
        # the function is defined outside of the dismo package
        brt.proj = predict(future.climate.scenario, brt.obj, n.trees=brt.obj$gbm.call$best.trees, type="response") # predict for given climate scenario
        saveModelProjection(brt.proj, "brt", "future") # save output
    } else {
        write(paste("FAIL!", species, "Cannot load brt.obj from", wd, "output_brt", sep=": "), stdout())
    }
}
