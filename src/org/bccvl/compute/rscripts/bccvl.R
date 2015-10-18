# FIXME: R env setup should be done on compute host
#        - lib dir: get rid of it
#        - don't do install.packages?
#        -
# setup R environment
#if (!file.exists(Sys.getenv("R_LIBS_USER"))) {
#    dir.create(Sys.getenv("R_LIBS_USER"), recursive=TRUE);
#}
#.libPaths(Sys.getenv("R_LIBS_USER"))
# set CRAN mirror in case we need to download something

## TODO: setup CRAN mirror in .Renviron
## see http://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html
## don't install here... just require

r <- getOption("repos")
r["CRAN"] <- "http://cran.ms.unimelb.edu.au/"
options(repos=r)
# print warnings immediately
options(warn=1)

# print out list of installed packages
write.table(installed.packages()[,c("Package", "Version", "Priority")],
            row.names=FALSE)

#script to run to develop distribution models
###check if libraries are installed, install if necessary and then load them
necessary=c("ggplot2","tools", "rjson", "dismo","SDMTools", "gbm", "rgdal", "pROC", "R2HTML", "png", "biomod2") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) {
    install.packages(necessary[!installed], dep = T) #if library is not installed, install it
}
for (lib in necessary) {
    library(lib,character.only=T) #load the libraries
}

# load parameters
params = rjson::fromJSON(file="params.json")
bccvl.params <- params$params
bccvl.env <- params$env
rm(params)

# set working directory (script runner takes care of it)
setwd(bccvl.env$outputdir)
# Set raster tmpdir - we do this because raster sometimes makes
# temp files (e.g. when cropping).
# Might want to make this configurable - e.g. we might want to
# control maxmemory, and/or other raster options
rasterOptions(tmpdir=paste(bccvl.env$workdir,"raster_tmp",sep="/"))

# Use seed supplied if any. Otherwise generate a random seed.
seed = bccvl.params$random_seed
if (is.null(seed)) {
    seed = runif(1, -2^31, 2^31-1)
}
seed = as.integer(seed)
set.seed(seed)
bccvl.params["random_seed"] = seed


############################################################
#
# define helper functions to print parameters
#
############################################################

parameter.as.string <- function (param, value) {
    pname <- gsub("_", " ", param)
    if (param == "prevalence") {
        pname = "weighted response weights"
    }
    else if (param == "var_import") {
        pname = "resampling"
    }
    else if (param == "nbcv") {
        pname = "NbCV"
    }
    else if (param == "n_trees") {
        pname = "trees added each cycle"
    }
    else if (param == "control_xval") {
        pname = "cross-validations"
    }
    else if (param == "control_minbucket") {
        pname = "minimum bucket"
    }
    else if (param == "control_minsplit") {
        pname = "minimum split"
    }
    else if (param == "control_cp") {
        pname = "complexity parameter"
    }
    else if (param == "control_maxdepth") {
        pname = "maximum depth"
    }
    else if (param == "irls_reg") {
        pname = "irls.reg"
    }
    else if (param == "maxit") {
        pname = "maximum iterations"
    }
    else if (param == "mgcv_tol") {
        pname = "convergence tolerance"
    }
    else if (param == "mgcv_half") {
        pname = "number of halvings"
    }
    else if (param == "n_minobsinnode") {
        pname = "Min observations in terminal node"
    }
    else if (param == "control_epsilon") {
        pname = "control: epsilon"
    }
    else if (param == "control_maxit") {
        pname = "control: maxit"
    }
    else if (param == "control_trace") {
        pname = "control: trace"
    }
    else if (param == "model") {
        pname = "Model returned"
    }
    else if (param == "x") {
        pname = "x returned"
    }
    else if (param == "y") {
        pname = "y returned"
    }
    else if (param == "qr") {
        pname = "QR returned"
    }
    else if (param == "singular_ok") {
        pname = "Singular fit ok"
    }
    else if (param == "thresh") {
        pname = "threshold"
    }
    else if (param == "maximumiterations") {
        pname = "Maximum iterations"
    }
    else if (param == "ntree") {
        pname = "number of trees"
    }
    else if (param == "mtry") {
        pname = "number of variables at each split (mtry)"
    }
    else if (param == "nodesize") {
        pname = "node size"
    }
    else if (param == "maxnodes") {
        pname = "maximum nodes"
    }
    return(paste(pname, " = ", value, "\n", sep="", collapse=""))
}

parameter.print <- function(params) {
    func = params[["function"]]
    if (is.null(func))
        return("")
    cat("Algorithm:", func, "\n")

    pnames = c("random_seed")
    if (func == "ann") {
        pnames = c("prevalence", "var_import", "maxit", "nbcv", "rang", "random_seed")
    }
    else if (func == "brt") {
        pnames = c("tree_complexity", "learning_rate", "bag_fraction", "n_folds", "prev_stratify", "family", "n_trees", "max_trees", "tolerance_method", "tolerance_value", "random_seed")
    }
    else if (func == "cta") {
        pnames = c("prevalence", "var_import", "method", "control_xval", "control_minbucket", "control_minsplit", "control_cp", "control_maxdepth", "random_seed")
    }
    else if (func == "fda") {
        pnames = c("prevalence", "var_import", "method", "random_seed")
    }
    else if (func == "gam") {
        pnames = c("prevalence", "var_import", "interaction_level", "family", "irls_reg", "epsilon", "maxit", "mgcv_tol", "mgcv_half", "random_seed")
    }
    else if (func == "gamlss") {
        pnames = c("sigma_formula", "nu_formula", "tau_formula", "family", "weights", "contrasts", "method", "start_from", "mu_start", "sigma_start", "nu_start", "tau_start", "mu_fix", "sigma_fix", "nu_fix", "tau_fix", "control", "i_control", "other_args", "random_seed")
    }
    else if (func == "gbm") {
        pnames = c("prevalence", "var_import", "distribution", "n_trees", "interaction_depth", "n_minobsinnode", "shrinkage", "bag_fraction", "train_fraction", "cv_folds", "random_seed")
    }
    else if (func == "glm") {
        pnames = c("prevalence", "var_import", "type", "interaction_level", "test", "family", "mustart", "control_epsilon", "control_maxit", "control_trace", "random_seed")
    }
    else if (func == "lm") {
        pnames = c("subset", "weights", "na_action", "method", "model", "x", "y", "qr", "singular_ok", "contrasts", "offset", "random_seed")
    }
    else if (func == "manova") {
        pnames = c("projections_returned", "qr", "contrasts", "subset", "weights", "na_action", "random_seed")
    }
    else if (func == "mars") {
        pnames = c("prevalence", "var_import", "degree", "nk", "penalty", "thresh", "prune", "random_seed")
    }
    else if (func == "maxent") {
        pnames = c("prevalence", "var_import", "maximumiterations", "linear", "quadratic", "product", "threshold", "hinge", "lq2lqptthreshold", "lq2lqthreshold", "hingethreshold", "beta_threshold", "beta_categorical", "beta_lqp", "beta_hinge", "defaultprevalence", "random_seed")
    }
    else if (func == "rf") {
        pnames = c("prevalence", "var_import", "do.classif", "ntree", "mtry", "nodesize", "maxnodes", "random_seed")
    }
    else if (func == "sre") {
        pnames = c("prevalence", "var_import", "quant", "random_seed")
    }

    for (p in pnames) {
        cat(parameter.as.string(p, params[[p]]))
    }
    return("")
}


# Print out parameters used
parameter.print(bccvl.params)


############################################################
#
# define helper functions to use in bccvl
#
############################################################

## Needed for tryCatch'ing:
bccvl.err.null <- function (e) return(NULL)

# read species presence/absence data
#    return NULL if filename is not  given
# TODO: shall we set projection here as well? use SpatialPoints?
bccvl.species.read <- function(filename) {
    if (!is.null(filename)) {
        # We might loose precision of lon/lat when ronverting to double,
        # However, given the nature of the numbers, and the resolution of raster files
        # we deal with, this shouldn't be a problem.
        return (read.csv(filename, colClasses=c("lon"="numeric", "lat"="numeric")))
    }
}

# use either absen.data (1) or generate random pseudo absence points (2)
# (1) extract "lat" and "lon" from absen.data
# (2) generate number of absence points in area of climate.data
bccvl.dismo.absence <- function(absen.data=NULL,
                                pseudo.absen.enabled=FALSE,
                                pseudo.absen.points=0,
                                climate.data=NULL,
                                occur.data=NULL) {
    # TODO: combine random and given absence points:
    # rbind(absen.datafromfile, bkgd.datarandom)
    if (pseudo.absen.enabled) {
        # generate randomPoints
        bkgd = randomPoints(
            climate.data,
            pseudo.absen.points,
            occur.data)
        # as data frame
        absen = as.data.frame(bkgd)
        # rename columns
        names(absen) <- c("lon","lat")
    } else {
        # otherwise read absence ponits from file
        absen = bccvl.species.read(absen.data) #read in the background position data lon.lat
        # keep only lon and lat columns
        absen = absen[c("lon","lat")]
    }
    return(absen);
}

# warning was doing odd things. I just want to print the deng thing.
bccvl.log.warning <-function(str, prefix="BCCVL Warning: ")
{
    print(paste(prefix, str, sep=""))
}


# rasters: a vector of rasters
bccvl.raster.common.extent <- function(rasters)
{
    extent.list=lapply(rasters, extent)
    # intersect all extents to find common extent
    common.extent = Reduce(intersect, extent.list)
                                        # compare all against commen extents to find out if all extents are the same (used to print warning)
    equal.extents = all(sapply(extent.list, function (x) common.extent == x))

    return (list(equal.extents=equal.extents, common.extent=common.extent))
}

# rasters: a vector of rasters
bccvl.raster.lowest.resolution <- function(rasters)
{
    res.list = lapply(rasters, res)

    lowest.res = c(max(sapply(res.list, function(x) x[[1]])),
                  max(sapply(res.list, function(x) x[[2]])))

    is.same.res = all(sapply(res.list, function (x) all(lowest.res == x)))

    return (list(lowest.res=lowest.res, is.same.res=is.same.res))
}

bccvl.raster.extent.to.str <- function(ext)
{
    return(sprintf("xmin=%f xmax=%f ymin=%f ymax=%f", ext@xmin, ext@xmax, ext@ymin, ext@ymax));
}


# raster.filenames : a vector of filenames that will be loaded as rasters
bccvl.rasters.to.common.extent.and.lowest.resolution <- function(raster.filenames)
{
    rasters = lapply(raster.filenames, raster)

    ce = bccvl.raster.common.extent(rasters)
    if (ce$equal.extents == FALSE)
    {
        bccvl.log.warning(sprintf("Auto cropping to common extent %s", bccvl.raster.extent.to.str(ce$common.extent)))
        rasters = lapply(rasters, function(x) crop(x, ce$common.extent))
    }

    lr=bccvl.raster.lowest.resolution(rasters)
    if (! lr$is.same.res)
    {
        bccvl.log.warning(sprintf("Auto resampling to lowest resolution [%f %f]", lr$lowest.res[[1]], lr$lowest.res[[2]]))
    }

    # get raster with lowest resolution
    master = Filter(function(x) all(res(x) == lr$lowest.res), rasters)[[1]]

    resamp_func <- function(x)
    {
        rsp = if (all(res(x) == lr$lowest.res)) x else resample(x, master)
        return(rsp)
    }

    return(lapply(rasters, resamp_func))

}

# return a RasterStack of given vector of input files
# intersecting extent
# lowest resolution
bccvl.enviro.stack <- function(filenames) {

    rasters = bccvl.rasters.to.common.extent.and.lowest.resolution(filenames)
    return(stack(rasters))
}

# function to save projection output raster
bccvl.saveModelProjection <- function(model.obj, projection.name, species, outputdir=bccvl.env$outputdir) {
    ## save projections under biomod2 compatible name:
    ##  proj_name_species.tif
    ##  only useful for dismo outputs
    basename = paste("proj", projection.name, species, sep="_")
    filename = file.path(outputdir, paste(basename, 'tif', sep="."))
    writeRaster(model.obj, filename, format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)
}

# function to save RData in outputdir
bccvl.save <- function(robj, name, outputdir=bccvl.env$outputdir) {
    filename = file.path(outputdir, name)
    save(robj, file=filename)
}

# function to save CSV Data in outputdir
bccvl.write.csv <- function(robj, name, outputdir=bccvl.env$outputdir) {
    filename = file.path(outputdir, name)
    write.csv(robj, file=filename)
}

# function to get model object
bccvl.getModelObject <- function(model.file=bccvl.env$inputmodel) {
    return (get(load(file=model.file)))
}

# convert all .gri/.grd found in folder to gtiff
# TODO: extend to handle other grid file formats, e.g. .asc
bccvl.grdtogtiff <- function(folder) {
    grdfiles <- list.files(path=folder,
                           pattern="^.*\\.gri")
    for (grdfile in grdfiles) {
        # get grid file name
        grdname <- file_path_sans_ext(grdfile)
        # read grid raster
        grd <- raster(file.path(folder, grdfile))

        if (is.na(proj4string(grd))) {
            # Projection is missing, initialise it to EPSG:4326
            crs = CRS("+init=epsg:4326")
            proj4string(grd) <- crs
        }

        # write raster as geotiff
        filename = file.path(folder, paste(grdname, 'tif', sep="."))
        writeRaster(grd, filename, format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)
        # remove grd files
        file.remove(file.path(folder, paste(grdname, c("grd","gri"), sep=".")))
    }
}

############################################################
#
# define helper functions for projections
#
############################################################

# function to check that the environmental layers used to project the
# model are the same as the ones used to create the model object
#    model.obj     ... model to project
#    climatelayers ... climate data to project onto
bccvl.checkModelLayers <- function(model.obj, climatelayers) {
    message("Checking environmental layers used for projection")
    # get the names of the environmental layers from the original model
    if (inherits(model.obj, "DistModel")) {
        # dismo package
        model.layers = colnames(model.obj@presence)
    } else if (inherits(model.obj, "gbm")) {
        # brt package
        model.layers = summary(model.obj)$var
    } else if (inherits(model.obj, "BIOMOD.models.out")) {
        # biomod package
        model.layers = model.obj@expl.var.names
    }

    # get the names of the climate scenario's env layers
    pred.layers = names(climatelayers)

    # check if the env layers were in the original model
    if(sum(!(pred.layers %in% model.layers)) > 0 ){
        message("Dropping environmental layers not used in the original model creation...")
        # create a new list of env predictors by dropping layers not in the original model
        new.predictors = climatelayers
        for (pl in pred.layers) {
            if (!(pl %in% model.layers)) {
                new.predictors = dropLayer(new.predictors, pl)
            }
        }
        return(new.predictors)
    } else {
        return(climatelayers)
    }
}



family_from_string <- function(s)
{
    # get family from a string (character) in a safe way
    # works for all variants of the R family object (e.g. see ?family)
    # i.e.
    # family_from_string("binomial")
    # family_from_string("binomial(link=logit)")
    # family_from_string("binomial(link=\"logit\")")
    # ...
    # family_from_string("quasi(link = \"identity\", variance = \"constant\")")

    s=gsub(pattern="\"|| ", replacement="", s) # strip quotes and spaces
    f=gsub(pattern="\\(.*\\)", replacement="", s) # the name of the function

    allowable= c("binomial",
                "gaussian",
                "Gamma",
                "inverse.gaussian",
                "poisson",
                "quasi",
                "quasibinomial",
                "quasipoisson")

    if (! f %in% allowable )
    {
        stop(sprintf("unsupported function %s", f))
    }

    fargs=gsub(pattern=".*\\(||\\)",
               replacement="",
               sub(pattern=f,
                    replacement="",
                    s)) #get the args inside the parentheses
    args=list()

    if (fargs != "")
    {
        l=strsplit(fargs, ",")[[1]]
        for( i in 1:length(l) )
        {
            ll=strsplit(l[i],"=")[[1]]
            if (length(ll) == 2)
            {
                args[ll[1]] = ll[2]
            }
            else
            {
                stop(sprintf("unhandled result when splitting %s", l[i]))
            }
        }
    }
    return (do.call(what=f, args=args))
}
