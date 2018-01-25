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
necessary=c("ggplot2","tools", "rjson", "dismo","SDMTools", "gbm", "rgdal", "rgeos", "pROC", "png", "gstat", "biomod2", "gdalUtils", "spatial.tools") #list the libraries needed
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
bccvl.species.read <- function(filename, month_filter=NULL) {
    if (!is.null(filename)) {
        # We might loose precision of lon/lat when ronverting to double,
        # However, given the nature of the numbers, and the resolution of raster files
        # we deal with, this shouldn't be a problem.
        csvfile = read.csv(filename, colClasses=c("lon"="numeric", "lat"="numeric"))
        if (is.null(month_filter)) {
            return(csvfile)
        }
        return(subset(csvfile, month %in% unlist(month_filter)))
    }
}

bccvl.data.transform <- function(data, climate.data)
{
    if (!is.null(data) & !compareCRS(data, climate.data, verbatim=TRUE)) {
        sp <- SpatialPoints(data)
        if (is.na(crs(sp))) {
            crs(sp) <- '+init=epsg:4326'
        }

        newdata <- as.data.frame(spTransform(sp, crs(climate.data)))
        names(newdata) <- c("lon", "lat")
        return(newdata)
    }
    return(data)
}

bccvl.format.outfilename <- function(filename, id_str, ext)
{
    return(sprintf("%s_%s.%s", filename, id_str, ext))
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
                                  save.pseudo.absen=TRUE,
                                  save.env.absen=TRUE,
                                  save.env.occur=TRUE,
                                  generate.background.data=FALSE,
                                  species_algo_str=NULL) {

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

    # Read true absence point if available.
    if (is.null(absen.filename)) {
        # create an empty data frame for bkgd points
        absen = data.frame(lon=numeric(0), lat=numeric(0))
        # To generate pseudo=absence points
        pseudo.absen.rep = 1
        if (!save.pseudo.absen) {
            pseudo.absen.rep = 0
        }
    }
    else {
        # read absence points from file
        absen = bccvl.species.read(absen.filename)
        # keep only lon and lat columns
        absen = absen[c("lon","lat")]

        # Ensure true absence dataset is in same projection system as climate.
        if (!is.null(climate.data) & !is.null(absen) & nrow(absen) > 0) {
            absen <- bccvl.data.transform(absen, climate.data)
        }

        # Do not generate pseudo absence point when true absence points are available
        pseudo.absen.rep = 0
        pseudo.absen.strategy = 'none'
        pseudo.absen.points = nrow(absen)
        cat("No pseudo absence point is generated.")
    }

    # Generate background data as pseudo absence points
    if (pseudo.absen.strategy != 'none' & generate.background.data) {
        biomod.data.pa <- c(rep(1, nrow(occur)), rep(0, nrow(absen)))
        myBackgrdData <-
            BIOMOD_FormatingData(resp.var  = biomod.data.pa,
                                 expl.var  = climate.data,
                                 resp.name = species.name,
                                 PA.nb.rep = pseudo.absen.rep,
                                 PA.nb.absences = pseudo.absen.points,
                                 PA.strategy = pseudo.absen.strategy,
                                 PA.dist.min = pseudo.absen.disk.min,
                                 PA.dist.max = pseudo.absen.disk.max,
                                 PA.sre.quant = pseudo.absen.sre.quant)

        # Get background data as absence data
        colnames(myBackgrdData@coord) <- c('lon', 'lat')
        absen <- myBackgrdData@coord[c(which(is.na(myBackgrdData@data.species))), c('lon', 'lat')]

        # Do not generate pa in next call to BIOMOD_FormatingData
        pseudo.absen.rep = 0
        pseudo.absen.strategy = 'none'
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
    pa_filename = bccvl.format.outfilename(filename="pseudo_absences", id_str=species_algo_str, ext="csv")
    absenv_filename = bccvl.format.outfilename(filename="absence_environmental", id_str=species_algo_str, ext="csv")
    occenv_filename = bccvl.format.outfilename(filename="occurrence_environmental", id_str=species_algo_str, ext="csv")
    if (pseudo.absen.rep != 0) {
        pseudoAbsen = myBiomodData@coord[c(which(is.na(myBiomodData@data.species))), c('lon', 'lat')]
        if (save.pseudo.absen & nrow(pseudoAbsen) > 0) {
            bccvl.write.csv(pseudoAbsen, pa_filename, rownames = FALSE)
        }

        # save the pseudo absence points with environmental variables
        if (save.env.absen) {
            bccvl.merge.save(climate.data, pseudoAbsen, species.name, absenv_filename)
        }
    }
    else if (nrow(absen) > 0) {
        # save true-absence/background data generated
        if (!is.null(absen.filename)) {
            # rename true-absence file
            pa_filename = bccvl.format.outfilename(filename="absence", id_str=species_algo_str, ext="csv")
        }
        bccvl.write.csv(absen, pa_filename, rownames = FALSE)
        
        # save the true absence points/background points with environmental variables
        if (save.env.absen) {
            bccvl.merge.save(climate.data, absen, species.name, absenv_filename)
        }
    }

    # save the occurrence datasets with environmental variables
    if (save.env.occur) {
        bccvl.merge.save(climate.data, occur, species.name, occenv_filename)
    }

    return(myBiomodData)
}

bccvl.merge.save <- function(env, csvdata, spname, ofname)
{
  data = cbind(csvdata, species=spname, extract(env, csvdata))

  bccvl.write.csv(data, ofname, rownames=FALSE)
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

bccvl.rasters.warp <- function(raster.filenames, raster.types, reference, overwrite=TRUE) {
    # This warping runs all the time,... while it is fairly fast, it probably can be skipped if all raster layers lign up correctly
    rasters = mapply(
        function(filename, filetype) {
            # Get the nodatavalue if available. Shall set to source's nodatavalue
            gdinfo = rgdal::GDALinfo(filename)
            mdata = attr(gdinfo, 'df')
            dtype = as.character(mdata[['GDType']])
            hasNoDataValues = mdata[['hasNoDataValue']]

            r = bccvl.raster.load(filename)
            # warp, crop and rescale raster file if necessary
            dir = dirname(filename)
            tmpf = file.path(dir, paste0(basename(tempfile()), '.tif')) # TODO: better filename and location?
            te = extent(reference)

            # This is to fix issue with NA value being treated as value 0 if nodatavalue is not set.
            if (hasNoDataValues) {
                # set nodatavalue to the original nodatavalue in the source file
                gdalwarp(filename, tmpf,
                         s_srs=CRSargs(crs(r)), t_srs=CRSargs(crs(reference)),
                         te=c(te@xmin, te@ymin, te@xmax, te@ymax),
                         ts=c(ncol(reference), nrow(reference)),
                         # tr=c(...), ... either this or ts
                         r="near",
                         of="GTiff",
                         dstnodata=mdata[['NoDataValue']],
                         co=c("TILED=YES", "COMPRESS=LZW")
                         )
            }
            else {
                # call gdalwarp without dstnodata
                gdalwarp(filename, tmpf,
                         s_srs=CRSargs(crs(r)), t_srs=CRSargs(crs(reference)),
                         te=c(te@xmin, te@ymin, te@xmax, te@ymax),
                         ts=c(ncol(reference), nrow(reference)),
                         # tr=c(...), ... either this or ts
                         r="near",
                         of="GTiff",
                         co=c("TILED=YES", "COMPRESS=LZW")
                         )
            }

            # put new file back into place
            rasterfilename = tmpf
            if (overwrite) {
                file.rename(tmpf, filename)
                rasterfilename = filename
            }
            # load new file and convert to categorical if required
            r = raster(rasterfilename)
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
bccvl.rasters.to.common.extent.and.resampled.resolution <- function(raster.filenames, raster.types, resamplingflag, overwrite=TRUE)
{
    # Load rasters and assign CRS if missing
    rasters = lapply(raster.filenames, bccvl.raster.load)

    # determine common raster shape
    reference = bccvl.rasters.common.reference(rasters, resamplingflag)

    # adjust rasters spatially and convert categorical rasters to factors
    rasters = bccvl.rasters.warp(raster.filenames, raster.types, reference, overwrite)

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

# Remove raster object and its associated raster files (i.e. grd and gri) if any
bccvl.remove.rasterObject <- function(rasterObject) {
    raster_filenames = raster_to_filenames(rasterObject, unique = TRUE)
    for (fname in raster_filenames) {
        if (extension(fname)  == '.grd') {
            file.remove(fname, extension(fname, '.gri'))
        }
    }
    rm(rasterObject)
}

bccvl.sp.transform <- function(data, climate.data)
{
    sp <- SpatialPoints(data)
    if (is.na(crs(sp))) {
        crs(sp) <- '+init=epsg:4326'
    }

    # project to the same crs as climate data
    if (!compareCRS(sp, climate.data, verbatim=TRUE)) {
        sp <- spTransform(sp, crs(climate.data))
    }
    return(sp)
}


# geographically constrained modelling
bccvl.sdm.geoconstrained <- function(rasterstack, occur, absenFilename, rawgeojson, generateCHull) {

    if (is.null(rawgeojson) & !generateCHull) {
        return(list("raster" = rasterstack, "occur" = occur))
    }

    # create a dummy geojson for convex-hull polygon if no geojson
    if (is.null(rawgeojson)) {
        parsedgeojson <- SpatialPolygons(list(Polygons(list(Polygon(rbind(c(1,1)))), ID=1)), proj4string=crs(rasterstack))
    } else {
        # Parse the geojson from text to SpatialPointsDataFrame
        parsedgeojson <- readOGR(dsn = rawgeojson, layer = "OGRGeoJSON", verbose = FALSE)
    }

    # Assign the same projection to the raster
    if (!compareCRS(rasterstack, parsedgeojson, verbatim=TRUE)) {
        # CRS is different, reproject geojson to rasterstack
        parsedgeojson <- spTransform(parsedgeojson, crs(rasterstack))
    }

    # If there are occurrence points, constraint them
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

        # actual constraint is the intersection between the occurrence's convex-hull polygon and the constraint.
        # Otherwise, actual constraint is the convex-hull polygon.
        if (generateCHull) {
            chcoords <- occurSP@coords[chull(occurSP@coords),]
            chullPolygon <- SpatialPolygons(list(Polygons(list(Polygon(chcoords, hole=FALSE)), ID=1)), proj4string=crs(parsedgeojson))
            if (!is.null(rawgeojson)) {
                parsedgeojson <- intersect(parsedgeojson, chullPolygon)
            }
            else {
                parsedgeojson <- chullPolygon
            }
        }

        # Add a small buffer of width 1-resolution cell. This is to fix the issue
        # with missing env values along the boundary of the polygon.
        parsedgeojson <- gBuffer(parsedgeojson, width=max(res(rasterstack@layers[[1]])))

        occurSPconstrained <- occurSP[!is.na(over(occurSP, parsedgeojson))]
        occurconstrained <- as.data.frame(occurSPconstrained)
        # rest of scripts expects names "lon", "lat" and not "x", "y"
        names(occurconstrained) <- c("lon", "lat")

        # constraint the true absence points if available
        if (!is.null(absenFilename)) {
            # read absence points from file
            absen = bccvl.species.read(absenFilename)
            # keep only lon and lat columns
            absen = absen[c("lon","lat")]

            # Ensure true absence dataset is in same projection system as climate 1st.
            if (!is.null(absen) & nrow(absen) > 0) {
                absenSP <- bccvl.sp.transform(absen, rasterstack)
                absenSPconstrained <- absenSP[!is.na(over(absenSP, parsedgeojson))]

                # project it back to epsg:4326 for saving as a csv file
                absenSPconstrained <- spTransform(absenSPconstrained, CRS('+init=epsg:4326'))
                absen <- as.data.frame(absenSPconstrained)
                names(absen) <- c("lon", "lat")
                write.csv(absen, file=absenFilename, row.names=FALSE)
            }
        }
    }
    else {
        occurconstrained = NULL
    }

    # Mask the rasterstack (and make sure it is a RasterStack)
    # Crop the raster to the extent of the constraint region before masking
    cropped_rasterstack <- crop(rasterstack, extent(parsedgeojson), filename = rasterTmpFile())

    # save the constrained raster in work directory instead of raster temporary directory as
    # predict.R clears the raster temp files.
    envraster_filename = paste(bccvl.env$workdir, basename(tempfile(fileext = ".grd")), sep="/")
    geoconstrained <- stack(mask(cropped_rasterstack, parsedgeojson, filename = envraster_filename))

    # Remove cropped rasterstack and associated raster files (i.e. grd and gri)
    bccvl.remove.rasterObject(cropped_rasterstack)

    # Return the masked raster stack and constrained occurrence points
    mylist <- list("raster" = geoconstrained, "occur" = occurconstrained)
    return(mylist)
}

# function to plot projection tiff file (with histogram)
bccvl.plotProjection <- function(inputfile, main) {
    ## define the breaks of the color key
    my.at <- seq(0,1.0,by=0.1)
    ## the labels will be placed vertically centered
    my.labs.at <- seq(0,1.0,by=0.25)
    ## define the labels
    my.lab <- seq(0,1.0,by=0.25)
    ## define colors
    my.col <- colorRampPalette(c("grey90","yellow4","green4"))(100)

    # Read in tiff input file as rasterstack and plot it
    require('rasterVis')
    levelplot(stack(raster(inputfile)),
              at=my.at,
              margin=T,
              col.regions=my.col,
              main=main,
              colorkey=list(labels=list(
                labels=my.lab,
                at=my.labs.at)))
}

# function to generate a filename for the specified file type and extension.
bccvl.get_filepath <- function(file_type, projection_name, species, outputdir=bccvl.env$outputdir, filename_ext=NULL, file_ext='tif') {
    if (is.null(filename_ext)) {
        basename = paste(file_type, projection_name, species, sep="_")
    }
    else {
        basename = paste(file_type, projection_name, species, filename_ext, sep="_")
    }
    return(file.path(outputdir, paste(basename, file_ext, sep=".")))
}

# function to save projection as png image
bccvl.saveProjectionImage <- function(inputfile, projection.name, species, species_algo_str, outputdir=bccvl.env$outputdir, filename_ext=NULL) {
    filename = bccvl.get_filepath("proj", projection.name, species_algo_str, outputdir, filename_ext, "png")
    png(filename)
    title = paste(species, projection.name, "projections", sep=" ")
    plot(raster(inputfile), main=title, xlab='longitude', ylab='latitude')
    # TODO: to use levelplot to produce histogram instead of plot.
    #bccvl.plotProjection(inputfile, title)
    dev.off()
}

# function to compute and save occurrence probability change metrics as geotif file
bccvl.generateOccurrenceProbChangeMetric <- function(prob_rasters, outfilename) {
    changeproj <- overlay(prob_rasters[[1]], prob_rasters[[2]], fun=function(r1, r2) { return(r1-r2) })
    writeRaster(changeproj, outfilename, format="GTiff", options=c("COMPRESS=LZW", "TILED=YES"), overwrite=TRUE)
}

# function to compute and save species range change metric as geotif file
bccvl.generateSpeciesRangeChangeMetric <- function(prob_rasters, threshold, outfilename) {
    # return 1 for Blank, 3 for Expansion, 0 for Contraction and 2 for No Change
    rangeChange <- overlay(as.integer(prob_rasters[[1]] > threshold),
                           as.integer(prob_rasters[[2]] > threshold),
                           fun=function(fp, cp) { return((2 * fp) + 1 - cp)})
    writeRaster(rangeChange, outfilename, format="GTiff", options=c("COMPRESS=LZW", "TILED=YES"), overwrite=TRUE)

    # compute the area for each change category
    grid_area <- raster.from.asc(grid.area(asc.from.raster(rangeChange)))
    total_pixels = ncell(na.omit(as.data.frame(rangeChange)))
    chg_summary = as.data.frame(matrix(ncol=3, nrow=4))
    rownames(chg_summary) <- c('Contraction', 'Blank', 'No Change', 'Expansion')
    colnames(chg_summary) <- c('no_grid_cells', '%_grid_cells', 'area_km2')
    for (i in c(0,1,2,3)) {
        no_pixels = length(rangeChange[rangeChange == i])
        chg_summary[i+1,] <- c(
                no_pixels,
                (no_pixels*100.0)/total_pixels,
                sum(grid_area[rangeChange == i])/1000000.0
            )
    }

    # write it to a file.
    outfilename2 = outfilename
    ext = file_ext(outfilename)
    if (!is.null(ext)) {
        pattern = paste0('\\.', ext, '$')
        outfilename2 <- sub(pattern, '', outfilename)
    }
    outfilename2 = paste(outfilename2, 'csv', sep=".")
    write.csv(chg_summary, file=outfilename2, row.names=TRUE)
}

# function to compute and save Centre of Gravity as csv file.
bccvl.generateCentreOfGravityMetric <- function(projfiles, outfilename) {
    future_proj = raster(projfiles[[1]])
    current_proj = raster(projfiles[[2]])
    future_cog = COGravity(future_proj)
    current_cog = COGravity(current_proj)

    results = as.data.frame(matrix(ncol=5, nrow=3))
    rownames(results) = c('Centre_of_Range', 'Minimum', 'Maximum')
    colnames(results) = c('current_latitude', 'current_longitude', 'future_latitude', 'future_longitude', 'change_in_km')
    results[1,] = distance(current_cog['COGy'], current_cog['COGx'], future_cog['COGy'], future_cog['COGx'])
    results[2,] = distance(min(coordinates(current_proj)[,2]),
                           min(coordinates(current_proj)[,1]),
                           min(coordinates(future_proj)[,2]),
                           min(coordinates(future_proj)[,1])
                          )
    results[3,] = distance(max(coordinates(current_proj)[,2]),
                           max(coordinates(current_proj)[,1]),
                           max(coordinates(future_proj)[,2]),
                           max(coordinates(future_proj)[,1])
                          )
    write.csv(results, file=outfilename)
}


# function to save projection output raster
bccvl.saveModelProjection <- function(model.obj, projection.name, species, species_algo_str, outputdir=bccvl.env$outputdir, filename_ext=NULL) {
    ## save projections under biomod2 compatible name:
    ##  proj_name_species.tif
    ##  only useful for dismo outputs

    filename = bccvl.get_filepath("proj", projection.name, species_algo_str, outputdir, filename_ext, "tif")
    writeRaster(model.obj, filename, format="GTiff", options=c("COMPRESS=LZW", "TILED=YES"), overwrite=TRUE)

    # TODO: can we merge this bit with bccvl.saveProjection in eval.R ?
    # Save as image as well
    pngfilename = bccvl.get_filepath("proj", projection.name, species_algo_str, outputdir, filename_ext, "png")
    png(pngfilename)
    title = paste(species, projection.name, "projections", sep=" ")
    plot(model.obj, xlab="latitude", ylab="longtitude", main=title)
    dev.off()

    return (filename)
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
bccvl.grdtogtiff <- function(folder, algorithm, filename_ext=NULL, noDataValue=NULL) {
    grdfiles <- list.files(path=folder,
                           pattern="^.*\\.grd")
    for (grdfile in grdfiles) {
        # get grid file name without the extension
        # file_path_sans_ext does not work when file has double '.' before extension i.e. filename..grd
        #grdname <- file_path_sans_ext(grdfile)
        ext = file_ext(grdfile)
        if (!is.null(ext)) {
            pattern = paste0('\\.', ext, '$')
            grdname <- sub(pattern, '', grdfile)
        }

        # read grid raster
        grd <- raster(file.path(folder, grdfile))

        if (is.na(proj4string(grd))) {
            # Projection is missing, initialise it to EPSG:4326
            crs = CRS("+init=epsg:4326")
            proj4string(grd) <- crs
        }

        # write raster as geotiff
        basename = paste(grdname, algorithm, sep="_")
        if (!is.null(filename_ext)) {
            basename = paste(grdname, algorithm, filename_ext, sep="_")
        }
        filename = file.path(folder, paste(basename, 'tif', sep="."))

        # To do: This is a temporary fix for nodatavalue is not recognised by mosaic_raster
        # due to a bug in gdal libarry. It shall be removed when using gdal 2.1.3.
        dtype = dataType(grd)
        if (is.null(noDataValue)) {
            writeRaster(grd, filename, datatype=dataType(grd),
                        format="GTiff", options=c("COMPRESS=LZW", "TILED=YES"), overwrite=TRUE)
        }
        else {
            writeRaster(grd, filename, datatype=dataType(grd), NAflag=noDataValue,
                        format="GTiff", options=c("COMPRESS=LZW", "TILED=YES"), overwrite=TRUE)
        }
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

#' Grid Information from Geographic (lat lon) Projections
#'
#' Since spatial grids in geographic projections do not have equal area or
#' perimeters, \code{grid.info} extracts perimeter & area related information
#' for latitudinal bands with differing longitudinal widths. \cr\cr Outputs
#' lengths are in m using Vincenty's equation (\code{distance})and areas in m2.
#' Surface areas are calculated summing surface areas of spherical polygons as
#' estimated using l'Huiller's formula.
#'
#'
#' @param lats is a vector of latitudes representing the midpoint of grid cells
#' @param cellsize is a single value (assuming square cells) or a two value
#' vector (rectangular cells) representing the height (latitude) and width
#' (longitude) of the cells
#' @param r is a single value representing the radius of the globe in m.
#' Default is for the WGS84 elipsoid
#' @return a data.frame listing: \item{lat}{the latitude representing the
#' midpoint of the cell} \item{top}{length of the top of the cell (m)}
#' \item{bottom}{length of the bottom of the cell (m)} \item{side}{length of
#' the side of the cell (m)} \item{diagnal}{length of the diagnals of the cell
#' (m)} \item{area}{area of the cell (m2)}
#' @author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}
#' @references information on l'Huiller's formula
#' \url{http://williams.best.vwh.net/avform.htm for more info)} code for
#' estimating area of polygon on sphere was modified from
#' \url{http://forum.worldwindcentral.com/showthread.php?t=20724}
#' @examples
#'
#' #show output for latitudes from -87.5 to 87.5 at 5 degree intervals
#' grid.info(lats=seq(-87.5,87.5,5), 5)
#'
#' @export
# This is a fix for SDM tool grid.info function due to floating point operation.
grid.info <- function(lats,cellsize,r=6378137) {
    r2 = r^2 #radius of earth
    ###need checks to ensure lats will not go beyond 90 & -90
    if (length(cellsize)==1) cellsize=rep(cellsize,2) #ensure cellsize is defined for both lat & lon
    out = data.frame(lat=lats) #setup the output dataframe
    toplats = lats+(0.5*cellsize[1]); bottomlats = lats-(0.5*cellsize[1]) #define the top and bottom lats
    check = range(c(toplats,bottomlats),na.rm=TRUE); if (-90.0001>check[1] | 90.0001<check[2]) stop('latitudes must be between -90 & 90 inclusively')
    out$top = distance(toplats,rep(0,length(lats)),toplats,rep(cellsize[2],length(lats)))$distance
    out$bottom = distance(bottomlats,rep(0,length(lats)),bottomlats,rep(cellsize[2],length(lats)))$distance
    out$side = distance(toplats,rep(0,length(lats)),bottomlats,rep(0,length(lats)))$distance
    out$diagnal = distance(toplats,rep(0,length(lats)),bottomlats,rep(cellsize[2],length(lats)))$distance
    #calculate area of a spherical triangle using spherical excess associated by knowing distances
    #tan(E/4) = sqrt(tan(s/2)*tan((s-a)/2)*tan((s-b)/2)*tan((s-c)/2))
    #where a, b, c = sides of spherical triangle
    #s = (a + b + c)/2
    #from CRC Standard Mathematical Tables
    #calculate excess based on  l'Huiller's formula (http://williams.best.vwh.net/avform.htm for more info)
    #code modified from (http://forum.worldwindcentral.com/showthread.php?t=20724)
    excess = function(lam1,lam2,beta1,beta2){ #calculate excess... inputs are in radians
        haversine = function(y) { (1-cos(y))/2 }
        cosB1 = cos(beta1); cosB2 = cos(beta2)
        hav1 = haversine(beta2-beta1) + cosB1*cosB2*haversine(lam2-lam1)
        aa = 2 * asin(sqrt(hav1)); bb = 0.5*pi - beta2; cc = 0.5*pi - beta1
        ss = 0.5*(aa+bb+cc)
        tt = tan(ss/2)*tan((ss-aa)/2)*tan((ss-bb)/2)*tan((ss-cc)/2)
        return(abs(4*atan(sqrt(abs(tt)))))
    }
    if (any(bottomlats==-90)) { pos = which(bottomlats==-90); bottomlats[pos] = -bottomlats[pos]; toplats[pos] = -toplats[pos]} #ensure no -90 bottom lats
    out$area = excess(lam1=0,lam2=cellsize[2]*pi/180,toplats*pi/180,toplats*pi/180)
    out$area = abs(out$area-excess(lam1=0,lam2=cellsize[2]*pi/180,bottomlats*pi/180,bottomlats*pi/180))*r2
    return(out)
}

#' Vincenty Direct Calculation of Distance and Direction
#'
#' \code{distance} estimates the distance given a starting & ending latitude
#' and longitude. \cr \cr For general information on Vincenty's formula, see
#' e.g., \url{http://en.wikipedia.org/wiki/Vincenty's_formulae}. It states: \cr
#' \emph{Vincenty's formulae are two related iterative methods used in geodesy
#' to calculate the distance between two points on the surface of an spheroid,
#' developed by Thaddeus Vincenty in 1975. They are based on the assumption
#' that the figure of the Earth is an oblate spheroid, and hence are more
#' accurate than methods such as great-circle distance which assume a spherical
#' Earth.} \cr \cr \bold{Note:} this method assumes a locations are lat & lon
#' given in WGS 84.\cr\cr Direction, if requested, is the the initial bearing
#' (sometimes referred to as forward azimuth) for which one would follow as a
#' straight line along a great-circle arc from start to finish.\cr \cr
#' \bold{Note:} this will fail if there are NA's in the data.
#'
#'
#' @param lat1 a single value or vector of values representing latitude in
#' decimal degrees from -90 to 90 degrees. Alternatively, a data.frame or
#' matrix can be used here with each column representing lat1, lon1, lat2, lon2
#' (in that order).
#' @param lon1 a single value or vector of values representing longitude in
#' decimal degrees from -180 to 180 degrees. If NULL, lat1 is assumed to be a
#' matrix or data.frame.
#' @param lat2 a single value or vector of values representing latitude in
#' decimal degrees from -90 to 90 degrees. If NULL, lat1 is assumed to be a
#' matrix or data.frame.
#' @param lon2 a single value or vector of values representing longitude in
#' decimal degrees from -180 to 180 degrees. If NULL, lat1 is assumed to be a
#' matrix or data.frame.
#' @param bearing boolean value as to calculate the direction as well as the
#' distance.
#' @return Returns a data.frame with: \item{lon1}{the original longitude}
#' \item{lat1}{the original latitude} \item{lon2}{the destination longitude}
#' \item{lat2}{the destination latitude} \item{distance}{the distance used}
#' \item{bearing}{if requested, the bearing between the two points}
#' @author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}
#' @seealso \code{\link{destination}}
#' @references Vincenty, T. 1975. Direct and Inverse Solutions of Geodesics on
#' the Ellipsoid with application of Nested Equations. Survey Review, vol XXII
#' no 176. \url{http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf}
#' @source The source code for the distance algorithm here was modified from
#' \url{http://www.movable-type.co.uk/scripts/latlong-vincenty.html}.\cr \cr
#' Distances were validated against Geoscience Australia calculations
#' (\url{http://www.ga.gov.au/geodesy/datums/vincenty_inverse.jsp}).\cr \cr
#' Bearings were from multiple sources including
#' \url{http://williams.best.vwh.net/avform.htm#Crs}.
#' @examples
#'
#'
#' #get the distance of 1 degree longitude at each 5 degrees latitude from -90 to 90
#' distance(lat1=seq(-90,90,5),lon1=rep(0,37),lat2=seq(-90,90,5),lon2=rep(1,37),bearing=TRUE)
#'
#'
#' @export
#' @useDynLib SDMTools Dist
# This is a fix for SDM tool distance function due to floating point operation.
distance = function(lat1, lon1=NULL, lat2=NULL, lon2=NULL, bearing=FALSE) {
    if (is.data.frame(lat1) | is.matrix(lat1)) { #if input is matrix or data.frame... break it out to individual vectors
        lat1 = as.matrix(lat1); if (ncol(lat1)!=4) stop('incorrect lat/lon inputs... must be matrix with 4 columns or 4 vectors')
        lon2=lat1[,4]; lat2=lat1[,3]; lon1=lat1[,2]; lat1=lat1[,1] #break out individual columns
    } else if (!is.null(lat2) & !is.null(lon1) & !is.null(lon2)) {
        if (!all(c(length(lat2),length(lon1),length(lon2))==length(lat1))) stop('inputs must all be of same length')
    } else { stop('inappropriate inputs... see helpfile') }
    if (any(c(lon1,lon2) < -180.0001) | any(c(lon1,lon2) > 180.0001)) stop('lon must be decimal degrees between -180 & 180')
    if (any(c(lat1,lat2) < -90.0001) | any(c(lat1,lat2) > 90.0001)) stop('lat must be decimal degrees between -90 & 90')
    #cycle through and output the new data
    out = data.frame(lat1=lat1,lon1=lon1,lat2=lat2,lon2=lon2)
    out$distance = round(.Call('Dist',out$lat1,out$lon1,out$lat2,out$lon2,PACKAGE='SDMTools'),2) #round to the nearest mm
    if (bearing) { #if requested, calculate bearing
        lat1=lat1*pi/180;lat2=lat2*pi/180;lon1=lon1*pi/180;lon2=lon2*pi/180 #convert to radians
        brng = atan2(sin(lon2-lon1)*cos(lat2),cos(lat1)*sin(lat2)-sin(lat1)*cos(lat2)*cos(lon1-lon2)) #estimate bearing
        out$bearing = ((brng*180/pi)+360)%%360 #convert to bearing in degrees
    }
    #return the output
    return(out)
}

assignInNamespace("grid.info",grid.info, ns="SDMTools")
assignInNamespace("distance",distance, ns="SDMTools")
