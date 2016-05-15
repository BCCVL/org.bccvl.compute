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
necessary=c("ggplot2","tools", "rjson", "dismo","SDMTools", "gbm", "rgdal", "pROC", "R2HTML", "png", "gstat", "biomod2", "gdalUtils") #list the libraries needed
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
    else if (param == "pa_ratio") {
        pname = "absence-presence ratio"
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

    pnames = c(pnames, "pa_ratio", "pa_strategy", "pa_sre_quant", "pa_disk_min", "pa_disk_max")
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

# BIOMOD_FormatingData(resp.var, expl.var, resp.xy = NULL, resp.name = NULL, eval.resp.var = NULL,
#   eval.expl.var = NULL, eval.resp.xy = NULL, PA.nb.rep = 0, PA.nb.absences = 1000, PA.strategy = 'random',
#   PA.dist.min = 0, PA.dist.max = NULL, PA.sre.quant = 0.025, PA.table = NULL, na.rm = TRUE)
#
# resp.var a vector, SpatialPointsDataFrame (or SpatialPoints if you work with `only presences' data) containing species data (a single species) in binary format (ones for presences, zeros for true absences and NA for indeterminated ) that will be used to build the species distribution models.
# expl.var a matrix, data.frame, SpatialPointsDataFrame or RasterStack containing your explanatory variables that will be used to build your models.
# resp.xy optional 2 columns matrix containing the X and Y coordinates of resp.var (only consider if resp.var is a vector) that will be used to build your models.
# eval.resp.var a vector, SpatialPointsDataFrame your species data (a single species) in binary format (ones for presences, zeros for true absences and NA for indeterminated ) that will be used to evaluate the models with independant data (or past data for instance).
# eval.expl.var a matrix, data.frame, SpatialPointsDataFrame or RasterStack containing your explanatory variables that will be used to evaluate the models with independant data (or past data for instance).
# eval.resp.xy opional 2 columns matrix containing the X and Y coordinates of resp.var (only consider if resp.var is a vector) that will be used to evaluate the modelswith independant data (or past data for instance).
# resp.name response variable name (character). The species name.
# PA.nb.rep number of required Pseudo Absences selection (if needed). 0 by Default.
# PA.nb.absences number of pseudo-absence selected for each repetition (when PA.nb.rep > 0) of the selection (true absences included)
# PA.strategy strategy for selecting the Pseudo Absences (must be `random', `sre', `disk' or `user.defined')
# PA.dist.min minimal distance to presences for `disk' Pseudo Absences selection (in meters if the explanatory is a not projected raster (+proj=longlat) and in map units (typically also meters) when it is projected or when explanatory variables are stored within table )
# PA.dist.max maximal distance to presences for `disk' Pseudo Absences selection(in meters if the explanatory is a not projected raster (+proj=longlat) and in map units (typically also meters) when it is projected or when explanatory variables are stored within table )
# PA.sre.quant quantile used for `sre' Pseudo Absences selection
# PA.table a matrix (or a data.frame) having as many rows than resp.var values. Each column correspund to a Pseudo-absences selection. It contains TRUE or FALSE indicating which values of resp.var will be considered to build models. It must be used with `user.defined' PA.strategy.
# na.rm logical, if TRUE, all points having one or several missing value for environmental data will be removed from analysis


# This uses the biomods function BIOMOD_FormatingData to format user input data. 
# It generates pseudo absence points if true absence data are not available or 
# adds pseudo absence data to an existing absence dataset.
bccvl.biomod2.formatData <- function(absen.filename=NULL,
                                  pseudo.absen.points=0,
                                  pseudo.absen.strategy='random',
                                  pseudo.absen.disk.min=0,
                                  pseudo.absen.disk.max=NULL,
                                  pseudo.absen.sre.quant = 0.025,
                                  climate.data=NULL,
                                  occur=NULL,
                                  species.name=NULL,
                                  save.pseudo.absen=TRUE) {

    # Read true absence point if available.
    if (is.null(absen.filename)) {        
        # create an empty data frame for bkgd points
        absen = data.frame(lon=numeric(0), lat=numeric(0))
    }
    else {
        # read absence points from file
        absen = bccvl.species.read(absen.filename)
        # keep only lon and lat columns
        absen = absen[c("lon","lat")]
    }

    # Initialise parameters to default value if not specified
    if (is.null(pseudo.absen.strategy)) {
        pseudo.absen.strategy = 'random'
    }
    if (is.null(pseudo.absen.disk.min)) {
        pseudo.absen.disk.min = 0
    }
    if (is.null(pseudo.absen.sre.quant)) {
        pseudo.absen.sre.quant = 0.025
    }

    # Check if we need to generate pseudo absence point here.
    # as BIOMOD_FormatingData() cannot handle it properly if
    # number of true absence points is more than absence
    # point required.
    pseudo.absen.rep = 1
    if (pseudo.absen.strategy == 'none' | nrow(absen) >=  pseudo.absen.points) {
        pseudo.absen.rep = 0
        cat("No pseudo absence point is generated.")
    }    

    biomod.data <- rbind(occur[,c("lon", "lat")], absen[,c("lon", "lat")])
    biomod.data.pa <- c(rep(1, nrow(occur)), rep(0, nrow(absen)))
    myBiomodData <-
        BIOMOD_FormatingData(resp.var  = biomod.data.pa,
                             expl.var  = climate.data,
                             resp.xy   = biomod.data,
                             resp.name = species.name,
                             PA.nb.rep = pseudo.absen.rep,
                             PA.nb.absences = pseudo.absen.points,
                             PA.strategy = pseudo.absen.strategy,
                             PA.dist.min = pseudo.absen.disk.min,
                             PA.dist.max = pseudo.absen.disk.max,
                             PA.sre.quant = pseudo.absen.sre.quant)

    # Save the pseudo absence points generated to file
    if (save.pseudo.absen) {
        pseudoAbsen = myBiomodData@coord[c(which(is.na(myBiomodData@data.species))), c('lon', 'lat')]
        if (nrow(pseudoAbsen) > 0) {
            bccvl.write.csv(pseudoAbsen, 'pseudo_absences.csv', rownames = FALSE)
        }
    }
    return(myBiomodData)
}

# warning was doing odd things. I just want to print the deng thing.
bccvl.log.warning <-function(str, prefix="BCCVL Warning: ")
{
    print(paste(prefix, str, sep=""))
}

bccvl.raster.load <- function(filename) {
    # load raster and assign crs if missing
    r = raster(filename)
    if (is.na(crs(r))) {
        crs(r) = CRS("+init=epsg:4326")
    }
    return(r)
}

# rasters: a vector of rasters, all rasters should have same resolution
# common.crs: crs to use to calculate intersection
bccvl.raster.common.extent <- function(rasters, common.crs)
{
    # bring all rasters into common crs
    extent.list = lapply(rasters, function(r) { extent(projectExtent(r, common.crs)) })
    # intersect all extents
    common.extent = Reduce(intersect, extent.list)
    # compare all against commen extents to find out if all extents are the same (used to print warning)
    equal.extents = all(sapply(extent.list, function (x) common.extent == x))

    return (list(equal.extents=equal.extents, common.extent=common.extent))
}

bccvl.raster.extent.to.str <- function(ext)
{
  return(sprintf("xmin=%f xmax=%f ymin=%f ymax=%f", ext@xmin, ext@xmax, ext@ymin, ext@ymax));
}

# rasters: a vector of rasters ... preferrably empty
# resamplingflag: a flag to determine which resampling approach to take
bccvl.rasters.common.resolution <- function(rasters, resamplingflag) {
    resolutions = lapply(rasters, res)
    if (resamplingflag == "highest") {
        common.res = Reduce(pmin, resolutions)
    } else if (resamplingflag == "lowest") {
        common.res = Reduce(pmax, resolutions)
    }
    is.same.res = all(sapply(resolutions, function(x) all(common.res == x)))
    return (list(common.res=common.res, is.same.res=is.same.res))
}

# generate reference raster with common resolutin, crs and extent
bccvl.rasters.common.reference <- function(rasters, resamplingflag) {
    # create list of empty rasters to speed up alignment
    empty.rasters = lapply(rasters, function(x) { projectExtent(x, crs(x)) })
    # choose a common.crs if all crs in rasters are the same use that one, otherwise use EPSG:4326 (common data in bccvl)
    common.crs = crs(empty.rasters[[1]])
    # TODO: print warning about reprojecting if necessary (if inside next condition)
    if (! do.call(compareRaster, c(empty.rasters, extent=FALSE, rowcol=FALSE, prj=TRUE, res=FALSE, orig=FALSE, rotation=FALSE, stopiffalse=FALSE))) {
        # we have different CRSs, so use EPSG:4326 as common
        # TODO: another strategy to find common CRS?
        common.crs = CRS("+init=epsg:4326")
        # project all rasters into common crs
        bccvl.log.warning(sprintf("Auto projecting to common CRS %s", common.crs))
        empty.rasters = lapply(empty.rasters, function(x) { projectExtent(x, common.crs) })
    }

    # determine commen.extent in common.crs
    # Note: extent is in projection units, -> rasters have to be in same CRS
    ce = bccvl.raster.common.extent(empty.rasters, common.crs)
    if (! ce$equal.extents) {
        bccvl.log.warning(sprintf("Auto cropping to common extent %s", bccvl.raster.extent.to.str(ce$common.extent)))
    }
    
    # determine common resolution
    # Note: resolution is usually in projection units. -> rasters should be in same CRS
    cr = bccvl.rasters.common.resolution(empty.rasters, resamplingflag)
    # TODO: print warning about resampling: common.res$is.same.res
    if (! cr$is.same.res) {
        bccvl.log.warning(sprintf("Auto resampling to %s resolution [%f %f]", resamplingflag, cr$common.res[[1]], cr$common.res[[2]]))
    }

    # apply common extent and resolution to empty rasters
    empty.rasters = lapply(
        empty.rasters,
        function(x) {
            extent(x) = ce$common.extent
            res(x) = cr$common.res
            return(x)
        })
    # from now an all empty.rasters should be exactly the same, pick first and return as
    # template.
    return(empty.rasters[[1]])
}

bccvl.rasters.warp <- function(raster.filenames, raster.types, reference) {
    rasters = mapply(
        function(filename, filetype) {

            r = bccvl.raster.load(filename)
            # warp, crop and rescale raster file if necessary
            dir = dirname(filename)
            tmpf = file.path(dir, 'tmp.tif') # TODO: better filename and location?
            te = extent(reference)
            gdalwarp(filename, tmpf,
                     s_srs=CRSargs(crs(r)), t_srs=CRSargs(crs(reference)),
                     te=c(te@xmin, te@ymin, te@xmax, te@ymax),
                     ts=c(ncol(reference), nrow(reference)),
                     # tr=c(...), ... either this or ts
                     r="near",
                     of="GTiff"
                     #co=c("TILED=YES", "COMPRESS=???")
                     )
            # put new file back into place
            file.rename(tmpf, filename)
            # load new file and convert to categorical if required
            r = raster(filename)
            if (filetype == "categorical") {
                # convert to factor if categorical
                r = as.factor(r)
            }
            return(r)
        },
        raster.filenames, raster.types)
    return(rasters)
}

# raster.filenames : a vector of filenames that will be loaded as rasters
# resamplingflag: a flag to determine which resampling approach to take
bccvl.rasters.to.common.extent.and.resampled.resolution <- function(raster.filenames, raster.types, resamplingflag)
{
    # Load rasters and assign CRS if missing
    rasters = lapply(raster.filenames, bccvl.raster.load)

    # determine common raster shape
    reference = bccvl.rasters.common.reference(rasters, resamplingflag)
    
    # adjust rasters spatially and convert categorical rasters to factors
    rasters = bccvl.rasters.warp(raster.filenames, raster.types, reference)

    return(rasters)
}

# return a RasterStack of given vector of input files
# intersecting extent
# lowest or highest resolution depending upon flag
bccvl.enviro.stack <- function(filenames, types, layernames, resamplingflag) {
    # adjust rasters to same projection, resolution and extent
    rasters = bccvl.rasters.to.common.extent.and.resampled.resolution(filenames, types, resamplingflag)
    # stack rasters
    rasterstack = stack(rasters)
    # assign predefined variable names
    names(rasterstack) = unlist(layernames)
    return(rasterstack)
}

# geographically constrained modelling
# bccvl.sdm.geoconstrained
bccvl.sdm.geoconstrained <- function(rasterstack, occur, rawgeojson) {
  
    # Parse the geojson from text to SpatialPointsDataFrame
    parsedgeojson <- readOGR(dsn = rawgeojson, layer = "OGRGeoJSON")
  
    # Assign the same projection to the raster 
    if (!compareCRS(rasterstack, parsedgeojson, verbatim=TRUE)) {
        # CRS is different, reproject geojson to rasterstack
        parsedgeojson <- spTransform(parsedgeojson, crs(rasterstack))
    }

    # Mask the rasterstack (and make sure it is a RasterStack)    
    geoconstrained <- stack(mask(rasterstack, parsedgeojson))

    # If there are occurrence points, constrain them
    if (!is.null(occur)) {
        # Constrain the occurrence points
        occurSP <- SpatialPoints(occur)
        # We have to make sure occurSP has the same CRS
        if (is.na(crs(occurSP))) {
            crs(occurSP) <- '+init=epsg:4326'
        }
        if (!compareCRS(occurSP, parsedgeojson, verbatim=TRUE)) {
            occurSP <- spTransform(occurSP, crs(parsedgeojson))
        }
        occurSPconstrained <- occurSP[!is.na(over(occurSP, parsedgeojson))]
        occurconstrained <- as.data.frame(occurSPconstrained)
        # rest of scripts expects names "lon", "lat" and not "x", "y"
        names(occurconstrained) <- c("lon", "lat")
    }
    else {
        occurconstrained = NULL
    }
    
    # Return the masked raster stack and constrained occurrence points
    mylist <- list("raster" = geoconstrained, "occur" = occurconstrained)
    return(mylist)
}

# function to save projection output raster
bccvl.saveModelProjection <- function(model.obj, projection.name, species, outputdir=bccvl.env$outputdir) {
    ## save projections under biomod2 compatible name:
    ##  proj_name_species.tif
    ##  only useful for dismo outputs
    basename = paste("proj", projection.name, species, sep="_")
    filename = file.path(outputdir, paste(basename, 'tif', sep="."))
    writeRaster(model.obj, filename, format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

    # TODO: can we merge this bit with bccvl.saveProjection in eval.R ?
    # Save as image as well
    png(file.path(outputdir, paste(basename, 'png', sep=".")))
    title = paste(species, projection.name, "projections", sep=" ")
    plot(model.obj, xlab="latitude", ylab="longtitude", main=title)
    dev.off()
}

# function to save RData in outputdir
bccvl.save <- function(robj, name, outputdir=bccvl.env$outputdir) {
    filename = file.path(outputdir, name)
    save(robj, file=filename)
}

# function to save CSV Data in outputdir
bccvl.write.csv <- function(robj, name, outputdir=bccvl.env$outputdir, rownames=TRUE) {
    filename = file.path(outputdir, name)
    write.csv(robj, file=filename, row.names=rownames)
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
bccvl.checkModelLayers <- function(model.obj, climatelayers, climate_filenames) {
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
    if (sum(!(pred.layers %in% model.layers)) > 0 ){
        # To do: Shall remove this sometimes later.
        # The model layer name is to be matched to climate layer name, or its file name.
        # This is to allow for old SDM result to be used.
        if (sum(!(model.layers %in% pred.layers)) > 0){
            filenames = lapply(climate_filenames, function(x) sub("^([^.]*).*", "\\1", basename(x)))
            indexes = match(model.layers, filenames)
            for (i in indexes){
                if (!is.na(i)){
                    pred.layers[i] = model.layers[i]    #Use the corresponding layer name in the model
                }
            }
            names(climatelayers) = pred.layers
        }

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
