# set CRAN mirror in case we need to download something
# TODO: this should be done on demand or on user basis... 
r <- getOption("repos") 
r["CRAN"] <- "http://cran.ms.unimelb.edu.au/"
options(repos=r)
# TODO: alse creating and populating add on package location is something that should not be done system wide

#script to run to develop distribution models
###check if libraries are installed, install if necessary and then load them
necessary=c("dismo","SDMTools", "rgdal") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) {
    install.packages(necessary[!installed], dep = T) #if library is not installed, install it
}
for (lib in necessary) {
    library(lib,character.only=T) #load the libraries
}

###read in the necessary observation, background and environmental data
#setwd(wd) #set the working directory
populate.data = FALSE #variable to define if there is a need to generate occur & background environmental info
if (file.exists(paste(wd, "/occur.RData", sep=""))
    && file.exists(paste(wd, "/lbkgd.RData", sep=""))) {
    load(paste(wd, "/occur.RData", sep=""))
    load(paste(wd, "/bkgd.RData", sep="")) #if files already exist, load in the data
    if (!all(colnames(occur)==c('lon','lat',enviro.data.names))) {
        populate.data=TRUE #not the right data, we need to repopulate it
    } 
} else {
    populate.data=TRUE # data does not exist, we need to generate it
} 
if (populate.data) {
    occur = read.csv(occur.data) #read in the observation data lon/lat
    if (!is.null(bkgd.data)) {
        bkgd = read.csv(bkgd.data) #read in teh background position data lon.lat
    }
    for (ii in 1:length(enviro.data)) {
        cat(ii,'of',length(enviro.data),'\n') #cycle through each of the environmental datasets and append the data
        tasc = read.asc(enviro.data[ii]) #read in the envirodata
        occur[,enviro.data.names[ii]] = extract.data(cbind(occur$lon,occur$lat),tasc) #extract envirodata for observations
        if (!is.null(bkgd.data)) bkgd[,enviro.data.names[ii]] = extract.data(cbind(bkgd$lon,bkgd$lat),tasc) #extract envirodata for background data
    }
    save(occur,file=paste(wd, "/occur.RData", sep="")) #write out the raw data for analysis
    if (!is.null(bkgd.data)) {
        save(bkgd,file=paste(wd, "/bkgd.RData", sep="")) #write out the raw data for analysis
    }
}

# combine predictors into a RasterStack of enviro data
cache.present = grep("maxent.cache", enviro.data)
if (length(cache.present) > 0) { # maxent.cache is present
    enviro.data = enviro.data[-cache.present]
}
climate.scenario = stack(enviro.data)

## Needed for tryCatch'ing:
err.null <- function (e) return(NULL)

# function to save projection output raster
saveModelProjection = function(out.model, model.name) {
    model.dir = paste(wd, "/output_", model.name, "/", sep="")
    writeRaster(out.model, paste(model.dir, "current", sep="/"), format="GTiff")
}

###run the models and store models
#################################################################################
#
# PROFILE METHODS - only consider presence points: Bioclim, Domain, and Mahal
#
#################################################################################

###############
#
# BIOCLIM
#
###############

# bioclim(x, p, ...)
# x is a Raster* object or matrix
# p is a two column matrix or SpatialPoints* object
# if p is missing, x is a matrix of values of env vars at known locations of occurrence
# if p is present, it is the location of occurrence and used to extract values for env vars from x,
#       a Raster* object
# NOTE: env vars must be numerical

if (model.bioclim) {
    if (!all(enviro.data.type=="continuous")) {
        warning("bioclim not run because categorical data cannot be used")
    } else {
        outdir = paste(wd,'/output_bioclim/',sep='')
        dir.create(outdir,recursive=TRUE) #create the output directory
        bc = tryCatch(bioclim(x=occur[,enviro.data.names]), error = err.null) #run bioclim with matrix of enviro data
        if (!is.null(bc)) {             
            save(bc,file=paste(outdir,"model.object.RData",sep='')) #save out the model object
            save(bc,file=paste(outdir,"model.object.Rascii",sep=''), ascii=TRUE) #save out the model object as ascii for Daniel
            bioclim.proj = predict(bc, climate.scenario, tails=opt.tails)   # predict for given climate scenario
            saveModelProjection(bioclim.proj, "bioclim") # save output
        } else {
            write(paste("FAIL!", species, "Cannot create bioclim model object", sep=": "), stdout())
        }                       
    } # end if continuous
} # end if
