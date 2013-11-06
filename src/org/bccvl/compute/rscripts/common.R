.libPaths(Sys.getenv("R_LIBS_USER"))
# set CRAN mirror in case we need to download something
# TODO: this should be done on demand or on user basis...
r <- getOption("repos")
r["CRAN"] <- "http://cran.ms.unimelb.edu.au/"
options(repos=r)

#script to run to develop distribution models
###check if libraries are installed, install if necessary and then load them
necessary=c("dismo","SDMTools", "gbm", "rgdal", "pROC") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) {
    install.packages(necessary[!installed], dep = T) #if library is not installed, install it
}
for (lib in necessary) {
    library(lib,character.only=T) #load the libraries
}


# TODO: alse creating and populating add on package location is something that should not be done system wide


## Needed for tryCatch'ing:
err.null <- function (e) return(NULL)

# read species presence/absence data
#    return NULL if filename is not  given
bccvl.species.read <- function(filename) {
    if (!is.null(filename)) {
        return (read.csv(filename))
    }
}

# function to save projection output raster
saveModelProjection <- function(model.obj, projection.name) {
    filename = file.path(outputdir, paste(projection.name, '.tif', sep=""))
    writeRaster(model.obj, filename, format="GTiff")
}

# function to get model object
getModelObject <- function(model.name) {
    filename = file.path(inputdir, paste(model.name, '.RData', sep=""))
    model.obj = tryCatch(get(load(file=filename)), error = err.null)
    return (model.obj)
}



# TODO: move to projection.R
# functions for assitance with projections
# function to check that the environmental layers used to project the
# model are the same as the ones used to create the model object
checkModelLayers = function(model.obj, climatelayers) {
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

    # get the names of the climater scenario's env layers
    # TODO: climatelayers ???? (future.climate.scenario??)
    pred.layers = names(climatelayers)

    # check if the env layers were in the original model
    if(sum(!(pred.layers %in% model.layers)) > 0 ){
        message("Dropping environmental layers not used in the original model creation...")
        # create a new list of env predictors by dropping layers not in the original model
        # TODO: climatelayers???? (future.climate.scenarios????)
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
