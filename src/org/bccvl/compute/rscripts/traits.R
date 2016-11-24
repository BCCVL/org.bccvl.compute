
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
necessary=c("ggplot2","tools", "rjson","SDMTools", "gbm", "raster", "rgdal", "R2HTML", "png", "gstat", "gdalUtils", "gam") #list the libraries needed
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

# Generate formulae for models that test the response of each trait to all environmental variables selected
# i.e. for trait diff model, trait ~ species 
# for trait cta/glm/gam models, trait ~ env1 + env2 + env3 etc 
bccvl.trait.gen_formulae <- function(dataset_params, trait_diff=FALSE) {
    cols = list(species=list(),
                lat=list(),
                lon=list(),
                env=list(),
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
        }
    }
    formulae = list()
    # For trait diff, variable is the species, otherwise environmental variables.
    variables = paste(names(cols[[ifelse(trait_diff, 'species', 'env')]]), collapse=' + ')
    for (trait in names(cols[['trait']])) {
        formulae = append(formulae, list(list(formula=paste(trait, '~', variables),
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
                             
# geographically constrained modelling
# return constrainted trait.data with env
bccvl.trait.constraint.merge <- function(trait.data, trait.params, raster.filenames, raster.types, layernames, rawgeojson) {

    # trait must have at least a trait
    if (is.null(trait.data)) {
        return(list("data" = trait.data, "params" = trait.params))
    }

    # Read in the raster, and assign predefined variable names
    rasters = list()
    if (length(raster.filenames) > 0) {
        rasters = mapply(bccvl.raster.load, raster.filenames, raster.types)
        names(rasters) = unlist(layernames)
    }

    # Parse the geojson from text to SpatialPointsDataFrame
    traitSP <- SpatialPoints(trait.data[c('lon', 'lat')])
    if (is.na(crs(traitSP))) {
        crs(traitSP) <- '+init=epsg:4326'
    }

    if (is.null(rawgeojson))
    {
        traitSPconstrained <- traitSP
    }
    else {
        parsedgeojson <- readOGR(dsn = rawgeojson, layer = "OGRGeoJSON")

        # CRS is different, reproject geojson to the trait coordinate systemf
        if (!compareCRS(traitSP, parsedgeojson, verbatim=TRUE)) {
            parsedgeojson <- spTransform(parsedgeojson, crs(traitSP))
        }
        # Constrain the occurrence points
        traitSPconstrained <- traitSP[!is.na(over(traitSP, parsedgeojson))]
    }

    trait.constrained <- as.data.frame(traitSPconstrained)
    names(trait.constrained) <- c("lon", "lat")

    # constraint the trait.data
    constrained.trait.data <- merge(trait.data, trait.constrained)

    if (length(rasters) > 0) {
        # Extract values from rasters, and combined with trait.data
        # Use constraint trait.data as the constraint to ensure same number of rows
        trait.constrained <- constrained.trait.data[c('lon', 'lat')]
        constrained.rasters <- sapply(rasters, extract, y = trait.constrained)
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
