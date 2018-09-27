
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

###check if libraries are installed, install if necessary and then load them
necessary=c("ggplot2","tools", "rjson","SDMTools", "gbm", "raster", "rgdal", "rgeos", "png", "gstat", "gdalUtils", "gam") #list the libraries needed
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
if (is.null(seed) || seed == "") {
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
    else if (param == "model") {
        pname = "Model returned"
    }
    else if (param == "x") {
        pname = "x returned"
    }
    else if (param == "y") {
        pname = "y returned"
    }
    else if (param == "nb_run_eval") {
        pname = "n-fold cross validation"
    }
    else if (param == "control_maxcompete") {
        pname = "number of competitor splits"
    }
    else if (param == "control_maxsurrogate") {
        pname = "number of surrogate splits"
    }
    else if (param == "control_usesurrogate") {
        pname = "surrogate usage"
    }
    else if (param == "control_surstyle") {
        pname = "surrogate style"
    }
    return(paste(pname, " = ", value, "\n", sep="", collapse=""))
}

# print out parameters used for STM algorithms: speciestrait_glm, speciestrait_gam, speciestrait_cta
parameter.print <- function(params) {
    func = params[["algorithm"]]
    if (is.null(func))
        return("")
    cat("Algorithm:", func, "\n")

    if (func == "speciestrait_glm") {
        pnames = c("family", "subset", "weights", "na_action", "start", "eta_start", "mu_start", "offset", "method", "model", "x", "y", "contrasts", "random_seed")
    }
    else if (func == "speciestrait_glmm") {
        # Todo: Need to update these parameters
        pnames = c("family", "subset", "weights", "na_action", "start", "eta_start", "mu_start", "offset", "method", "model", "x", "y", "contrasts", "random_seed")
    }
    else if (func == "speciestrait_gam") {
        pnames = c("family", "subset", "weights", "na_action", "start", "eta_start", "mu_start", "method", "model", "x", "y", "random_seed")
    }
    else if (func == "speciestrait_cta") {
        pnames = c("control_xval", "control_minbucket", "control_minsplit", "control_cp", "control_maxdepth", "control_maxcompete", "control_maxsurrogate", "control_usesurrogate", "control_surstyle", "random_seed")
    }
    else if (func == "traitdiff_glm") {
        # Todo: Need to update these parameters
        pnames = c("family", "subset", "weights", "na_action", "start", "eta_start", "mu_start", "offset", "method", "model", "x", "y", "contrasts", "random_seed")
    }
    else if (func == "exploration_plot") {
        pnames = c("random_seed")
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
# define helper functions to use for species trait experiment
#
############################################################

## Needed for tryCatch'ing:
bccvl.err.null <- function (e) return(NULL)

# warning was doing odd things. I just want to print the deng thing.
bccvl.log.warning <-function(str, prefix="BCCVL Warning: ")
{
    print(paste(prefix, str, sep=""))
}

# Generate formulae for models that test the response of each trait to all environmental variables selected
# i.e. for trait diff model, trait ~ species
# for trait cta/glm/gam models, trait ~ env1 + env2 + env3 etc
# for trait glmm, trait ~ env1 + env2 + (1|random1) + (1|random2)
bccvl.trait.gen_formulae <- function(dataset_params, trait_diff=FALSE, include_rf=FALSE) {
    cols = list(species=list(),
                lat=list(),
                lon=list(),
                env=list(),
                random=list(),
                trait=list())
    for(colname in names(dataset_params)) {
        colval = dataset_params[[colname]]
        if (colval == 'species' || colval == 'lon' || colval == 'lat') {
            cols[[colval]][colname] = colval
        } else if (colval == 'env_var_cat') {
            cols[['env']][colname] = 'categorical'
        } else if (colval == 'env_var_con') {
            cols[['env']][colname] = 'continuous'
        } else if (colval == 'trait_ord') {
            cols[['trait']][colname] = 'ordinal'
        } else if (colval == 'trait_nom') {
            cols[['trait']][colname] = 'nominal'
        } else if (colval == 'trait_con') {
            cols[['trait']][colname] = 'continuous'
        } else if (colval == 'random_cat') {
            cols[['random']][colname] = 'categorical'
        } else if (colval == 'random_con') {
            cols[['random']][colname] = 'continuous'
        }
    }

    formulae = list()
    # For trait diff, variable is the species, otherwise environmental variables.
    variables = paste(names(cols[[ifelse(trait_diff, 'species', 'env')]]), collapse=' + ')

    # only include random factors for glmm and cta
    ranvarnames = NULL
    randomvars = ""
    if (include_rf) {
        ranvarnames = names(cols[['random']])
        if (!is.null(ranvarnames)) {
            randomvars = paste("(1|", ranvarnames, ")", sep = '', collapse=' + ')
        }
    }
    for (trait in names(cols[['trait']])) {
        formula = ifelse(is.null(ranvarnames),
                         paste(trait, '~', variables),
                         paste(trait, '~', variables, '+', randomvars))
        formulae = append(formulae, list(list(formula=formula,
                                    type=cols[['trait']][trait],
                                    trait=trait)))
    }
    # return a list of lists, where each sublist has $formula, $type, and $trait
    return (formulae)
}

bccvl.raster.load <- function(filename, filetype = 'continuous') {
    # load raster and assign crs if missing
    r = raster(filename)
    if (is.na(crs(r))) {
        crs(r) = CRS("+init=epsg:4326")
    }

    if (filetype == "categorical") {
        # convert to factor if categorical
        r = as.factor(r)
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
# selected_layers: a list of indexes to the raster layers to be considered when determine the resolution to be used.
bccvl.rasters.common.resolution <- function(rasters, resamplingflag, selected_layers) {
    # Return the resolution of the raster given by the index
    get.resolution <- function(i, rasters)
    {
        return(res(rasters[[i]]))
    }

    # Get resolutions of the raster layers
    if (is.null(selected_layers)) {
        resolutions = lapply(rasters, res)
    }
    else {
        # get the resolutions of the of the selected raster layers only 
        resolutions = lapply(selected_layers, get.resolution, rasters)
    }
    
    #resolutions = lapply(rasters, res)
    if (resamplingflag == "highest") {
        common.res = Reduce(pmin, resolutions)
    } else if (resamplingflag == "lowest") {
        common.res = Reduce(pmax, resolutions)
    }

    # Get resolutions of all input raster layers
    resolutions = lapply(rasters, res)
    is.same.res = all(sapply(resolutions, function(x) all(common.res == x)))
    return (list(common.res=common.res, is.same.res=is.same.res))
}

# generate reference raster with common resolutin, crs and extent
bccvl.rasters.common.reference <- function(rasters, resamplingflag, selected_layers) {
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
    cr = bccvl.rasters.common.resolution(empty.rasters, resamplingflag, selected_layers)
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
bccvl.rasters.to.common.extent.and.resampled.resolution <- function(raster.filenames, raster.types, resamplingflag, selected_layers=NULL, overwrite=TRUE)
{
    # Load rasters and assign CRS if missing
    rasters = lapply(raster.filenames, bccvl.raster.load)

    # determine common raster shape
    reference = bccvl.rasters.common.reference(rasters, resamplingflag, selected_layers)

    # adjust rasters spatially and convert categorical rasters to factors
    rasters = bccvl.rasters.warp(raster.filenames, raster.types, reference, overwrite)

    return(rasters)
}

# return a RasterStack of given vector of input files
# intersecting extent
# lowest or highest resolution depending upon flag
# selected_layers is a list of layers to be considered when determine the resolution of the raster. If none, consider all layers.
bccvl.enviro.stack <- function(filenames, types, layernames, resamplingflag, selected_layers=NULL) {
    # adjust rasters to same projection, resolution and extent
    rasters = bccvl.rasters.to.common.extent.and.resampled.resolution(filenames, types, resamplingflag, selected_layers)
    # stack rasters
    rasterstack = stack(rasters)
    # assign predefined variable names
    names(rasterstack) = unlist(layernames)
    return(rasterstack)
}

# geographically constrained modelling
# return constrainted trait.data with env
bccvl.trait.constraint.merge <- function(trait.data, trait.params, raster.filenames, raster.types, layernames, rawgeojson, generateCHull) {

    # trait must have at least a trait
    if (is.null(trait.data) & !generateCHull) {
        return(list("data" = trait.data, "params" = trait.params))
    }

    # Load the environmental raster layers
    rasterstack = bccvl.enviro.stack(raster.filenames, raster.types, layernames, "highest")

    # Parse the geojson from text to SpatialPointsDataFrame
    traitSP <- SpatialPoints(trait.data[c('lon', 'lat')])
    if (is.na(crs(traitSP))) {
        crs(traitSP) <- '+init=epsg:4326'
    }

    # 
    if (!compareCRS(traitSP, rasterstack, verbatim=TRUE)) {
        traitSP <- spTransform(traitSP, crs(rasterstack))
    }


    if (is.null(rawgeojson))
    {
        traitSPconstrained <- traitSP
    }
    else {
        # Make geojson a SpatialPolygons object due to storing constraint method info as properties
        parsedgeojson <- readOGR(dsn = rawgeojson, layer = "OGRGeoJSON", verbose = FALSE)
        parsedgeojson <- SpatialPolygons(parsedgeojson@polygons, proj4string=parsedgeojson@proj4string)

        # CRS is different, reproject geojson to the trait coordinate system
        if (!compareCRS(traitSP, parsedgeojson, verbatim=TRUE)) {
            parsedgeojson <- spTransform(parsedgeojson, crs(traitSP))
        }

        if (generateCHull) {
            # Generate convex-hull polygon as constraint
            # get the offset 
            constraintjson <- rjson::fromJSON(rawgeojson)
            region_offset <- constraintjson$properties$region_offset
            if (is.null(region_offset) || is.na(region_offset) || region_offset == '') {
                region_offset = 0
            }
            else {
                region_offset <- as.double(region_offset)
                region_offset <- ifelse(!is.na(region_offset) && is.numeric(region_offset), region_offset/111.0, 0) # convert from km to degree
            }

            chcoords <- traitSP@coords[chull(traitSP@coords[,1:2]),]
            chullPolygon <- SpatialPolygons(list(Polygons(list(Polygon(chcoords[,1:2], hole=FALSE)), ID=1)), proj4string=crs(parsedgeojson))

            # intersection of convex-hull polygon and constraint region
            parsedgeojson <- intersect(parsedgeojson, chullPolygon)

            # Add a small buffer of width 1-resolution cell. This is to fix the issue
            # with missing env values along the boundary of the polygon.
            parsedgeojson <- gBuffer(parsedgeojson, width=max(region_offset, max(res(rasterstack))))
        }

        # Constrain the occurrence points
        traitSPconstrained <- traitSP[!is.na(over(traitSP, parsedgeojson))]
    }

    trait.constrained <- as.data.frame(traitSPconstrained)
    names(trait.constrained) <- c("lon", "lat")

    # constraint the trait.data
    constrained.trait.data <- merge(trait.data, unique(trait.constrained))

    if (length(rasterstack@layers) > 0) {
        # Extract values from rasters, and combined with trait.data
        # Use constraint trait.data as the constraint to ensure same number of rows
        trait.constrained <- constrained.trait.data[c('lon', 'lat')]
        constrained.rasters <- extract(rasterstack, y = trait.constrained)
        constrained.trait.data <- cbind(constrained.trait.data, constrained.rasters)

        # Update the trait dataset parameters with environmental variables types
        for (i in 1:length(layernames)) {
          colname <- layernames[[i]]
          trait.params[colname] = ifelse(raster.types[[i]] == 'continuous', 'env_var_con', 'env_var_cat')
        }
    }

    # Convert column as factor for ordinal and norminal trait data
    for (colname in names(trait.params)) {
        colval = trait.params[[colname]]
        if (colval == 'trait_ord' || colval == 'trait_nom') {
            constrained.trait.data[colname] = factor(constrained.trait.data[[colname]])
        }
    }

    # save the trait-environmental data as csv
    traitenv_filename = sprintf("%s_trait_environmental.csv", constrained.trait.data['species'][1,1])
    bccvl.write.csv(constrained.trait.data, traitenv_filename)
    return(list("data" = constrained.trait.data, "params" = trait.params))
}

# function to save projection output raster
bccvl.saveModelProjection <- function(model.obj, projection.name, species, outputdir=bccvl.env$outputdir, filename_ext=NULL) {
    ## save projections under biomod2 compatible name:
    ##  proj_name_species.tif
    ##  only useful for dismo outputs

    if (is.null(filename_ext)) {
        basename = paste("proj", projection.name, species, sep="_")
    }
    else {
        basename = paste("proj", projection.name, species, filename_ext, sep="_")
    }
    filename = file.path(outputdir, paste(basename, 'tif', sep="."))
    writeRaster(model.obj, filename, format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

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

bccvl.write.image <- function(robj, name, plotfn, outputdir=bccvl.env$outputdir) {
    png(file.path(outputdir, paste(name, 'png', sep=".")))
    plot.function = paste0(plotfn, "(robj)")
    eval(parse(text=plot.function))
    dev.off()
}

bccvl.write.text <- function(robj, name, outputdir=bccvl.env$outputdir, append=FALSE) {
    filename = file.path(outputdir, name)
    capture.output(robj, file=filename, append=append)
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
bccvl.grdtogtiff <- function(folder, filename_ext=NULL) {
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
        basename = grdname
        if (!is.null(filename_ext)) {
            basename = paste(grdname, filename_ext, sep="_")
        }
        filename = file.path(folder, paste(basename, 'tif', sep="."))
        writeRaster(grd, filename, format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)
        # remove grd files
        file.remove(file.path(folder, paste(grdname, c("grd","gri"), sep=".")))
    }
}


# Return a proper family object from string specified.
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

### Author: Jon Shuker

## Note: Exception handling will need to be added to this script if needed.

bccvl.regPlots = function(regResult, outerTitle="", fnamePrefix="", outputdir=bccvl.env$outputdir) {
  
  plotWidth = 7; plotHeight = 7  # inches
  plotDPI = 300

  listVars = names(regResult$coefficients)[-1]   # exclude Intercept which is first
  nVars = length(listVars)
  nCols = ceiling(sqrt(nVars))
  nRows = ceiling(nVars/nCols)
  
  png(filename=file.path(outputdir, paste0(fnamePrefix, 'plots.png')),
      width=plotWidth, height=plotHeight, unit='in', res=plotDPI)
  par(mfrow=c(nRows,nCols), oma=c(0,0,1.5,0))
  require(visreg)
  for (nameVar in listVars) {
    visreg(regResult, nameVar, ylab="")
    # visreg(regResult, "B01")
  }
  mtext(outerTitle, outer=TRUE, line=0)
  dev.off()

  png(filename=file.path(outputdir, paste0(fnamePrefix, 'diagnostic_plots.png')),
      width=plotWidth, height=plotHeight, unit='in', res=plotDPI)
  layout(matrix(c(1,3,2,4),2,2)) # 4 graphs/page
  plot(regResult)
  title(outerTitle, outer=TRUE, line=-1)
  dev.off()
  
}


bccvl.exploratoryPlots.categ = function(dataToExplore, outerTitle="", fnamePrefix="",
                                    outputdir=bccvl.env$outputdir) {
  
  plotWidth = 7; plotHeight = 7  # inches
  plotDPI = 300

  listVars = colnames(dataToExplore)
  nVars = length(listVars)
  nCols = ceiling(sqrt(nVars))
  nRows = ceiling(nVars/nCols)
  
  # Barplots
  
  png(filename=file.path(outputdir, paste0(fnamePrefix, 'barplot.png')),
      width=plotWidth, height=plotHeight, unit='in', res=plotDPI)
      # width=800, height=800, unit='px', 
      # res=300, pointsize=12*(800/480)*(72/300))  ##, type='cairo')
  par(mfrow=c(nRows,nCols), oma=c(0,0,1.5,0))
  for (nameVar in listVars) {
    barplot(table(dataToExplore[,c(nameVar)]), 
            main=nameVar)
  }
  mtext(outerTitle, outer=TRUE, line=0)
  dev.off()
  
}


bccvl.exploratoryPlots.contin = function(dataToExplore, doCorrelation=FALSE, outerTitle="",
                                   fnamePrefix="", outputdir=bccvl.env$outputdir) {
  
  listVars = colnames(dataToExplore)
  nVars = length(listVars)
  nCols = ceiling(sqrt(nVars))
  nRows = ceiling(nVars/nCols)
  
  plotWidth = 7; plotHeight = 7  # inches
  plotDPI = 300

  # Boxplots
  
  png(filename=file.path(outputdir, paste0(fnamePrefix, 'boxplot.png')),
      width=plotWidth, height=plotHeight, unit='in', res=plotDPI)
      # width=800, height=800, unit='px', 
      # res=300, pointsize=12*(800/480)*(72/300))  ##, type='cairo')
  par(mfrow=c(nRows,nCols), oma=c(0,0,1.5,0))
  for (nameVar in listVars) {
    boxplot(dataToExplore[,c(nameVar)], main=nameVar)
  }
  mtext(outerTitle, outer=TRUE, line=0)
  dev.off()
  
  # Histograms
  
  png(filename=file.path(outputdir, paste0(fnamePrefix, 'histogram.png')),
      width=plotWidth, height=plotHeight, unit='in', res=plotDPI)
  par(mfrow=c(nRows,nCols), oma=c(0,0,1.5,0))
  for (nameVar in listVars) {
    hist(dataToExplore[,c(nameVar)], breaks=10, xlab="", main=nameVar)
  }
  mtext(outerTitle, outer=TRUE, line=0)
  dev.off()

  # Density plots
  
  png(filename=file.path(outputdir, paste0(fnamePrefix, 'density.png')),
      width=plotWidth, height=plotHeight, unit='in', res=plotDPI)
  par(mfrow=c(nRows,nCols), oma=c(0,0,1.5,0))
  for (nameVar in listVars) {
    plot(density(dataToExplore[,c(nameVar)], na.rm=TRUE), main=nameVar)
  }
  mtext(outerTitle, outer=TRUE, line=0)
  dev.off()
  
  # Correlations, if requested
  
  if (doCorrelation && nCols > 1) {
    
    png(filename=file.path(outputdir, paste0(fnamePrefix, 'correlations.png')),
        width=plotWidth, height=plotHeight, unit='in', res=plotDPI)
    par(oma=c(0,0,1.5,0))
    pairs(dataToExplore, 
          lower.panel = function(x,y){
            usrSaved = par("usr"); on.exit(par(usrSaved))
            par(usr = c(0, 1, 0, 1))
            text(0.5, 0.5, 
                 round(cor(x, y, use="na.or.complete", method="pearson"), digits=2))})
    mtext(outerTitle, outer=TRUE, line=0)
    dev.off()
    
  }
  
}
