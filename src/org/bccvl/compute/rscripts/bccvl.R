# FIXME: R env setup should be done on compute host
#        - lib dir: get rid of it
#        - don't do install.packages?
#        -
# setup R environment
if (!file.exists(Sys.getenv("R_LIBS_USER"))) {
    dir.create(Sys.getenv("R_LIBS_USER"), recursive=TRUE);
}
.libPaths(Sys.getenv("R_LIBS_USER"))
# set CRAN mirror in case we need to download something

r <- getOption("repos")
r["CRAN"] <- "http://cran.ms.unimelb.edu.au/"
options(repos=r)


#script to run to develop distribution models
###check if libraries are installed, install if necessary and then load them
necessary=c("tools", "rjson", "dismo","SDMTools", "gbm", "rgdal", "pROC", "R2HTML", "png", "biomod2") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) {
    install.packages(necessary[!installed], dep = T) #if library is not installed, install it
}
for (lib in necessary) {
    library(lib,character.only=T) #load the libraries
}

# load parameters
bccvl.params <- rjson::fromJSON(file="params.json")
# set working directory (script runner takes care of it)
setwd(bccvl.params$outputdir)

############################################################
#
# define helper functions to use in bccvl
#
############################################################

## Needed for tryCatch'ing:
bccvl.err.null <- function (e) return(NULL)

# read species presence/absence data
#    return NULL if filename is not  given
bccvl.species.read <- function(filename) {
    if (!is.null(filename)) {
        return (read.csv(filename))
    }
}

# function to save projection output raster
bccvl.saveModelProjection <- function(model.obj, projection.name, outputdir=bccvl.params$outputdir) {
    ## save projections under biomod2 compatible name:
    ##  proj_name_species.tif
    ##  only useful for dismo outputs
    basename = paste("proj", projection.name, bccvl.params$species, sep="_")
    filename = file.path(outputdir, paste(basename, 'tif', sep="."))
    writeRaster(model.obj, filename, format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)
}

# function to save RData in outputdir
bccvl.save <- function(robj, name, outputdir=bccvl.params$outputdir) {
    filename = file.path(outputdir, name)
    save(robj, file=filename)
}

# function to save CSV Data in outputdir
bccvl.write.csv <- function(robj, name, outputdir=bccvl.params$outputdir) {
    filename = file.path(outputdir, name)
    write.csv(robj, file=filename)
}

# function to get model object
bccvl.getModelObject <- function(model.file=bccvl.params$inputmodel) {
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
        # write raster as geotiff
        outputdir = bccvl.params$outputdir
        filename = file.path(outputdir, paste(grdname, 'tif', sep="."))
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
