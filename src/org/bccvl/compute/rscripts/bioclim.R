####
##
##  INPUT:
##
##  occur.data ... filename for occurence data
##  bkgd.data  ... filename for absence data
##  enviro.data.current ... list of filenames for climate data
##  enviro.data.type    ... continuous
##  opt.tails ... predict parameter
##
##  outputdir ... root folder for output data


###read in the necessary observation, background and environmental data
occur = bccvl.species.read(occur.data) #read in the observation data lon/lat
bkgd = bccvl.species.read(bkgd.data) #read in the background position data lon.lat
# keep only lon and lat columns
occur = occur[c("lon","lat")]
bkgd = bkgd[c("lon","lat")]

# prepare current climate data
current.climate.scenario = stack(enviro.data.current)

# extract enviro data for species observation points and append to species data
occur = cbind(occur, extract(current.climate.scenario, cbind(occur$lon, occur$lat)))
if (!is.null(bkgd)) {
    bkgd = cbind(bkgd, extract(current.climate.scenario, cbind(bkgd$lon, bkgd$lat)))
}


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

if (!all(enviro.data.type=="continuous")) {
    warning("bioclim not run because categorical data cannot be used")
} else {
    # run bioclim with matrix of enviro data
    bc = tryCatch(bioclim(x=occur[,names(current.climate.scenario)]), error=err.null)
    if (!is.null(bc)) {
        # save out the model object
        save(bc, file=file.path(outputdir, "model.object.RData"))
        # predict for given climate scenario
        bioclim.proj = predict(bc, current.climate.scenario, tails=opt.tails)
        # save output
        saveModelProjection(bioclim.proj, "current")
        # evaluate model
        if (!is.null(bkgd)) {
            evaluate.model('bioclim', bc, occur, bkgd)
        }
    } else {
        write(paste("FAIL!", species, "Cannot create bioclim model object", sep=": "), stdout())
    }
} # end if continuous
