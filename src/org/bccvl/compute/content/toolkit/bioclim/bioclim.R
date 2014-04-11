####
##
##  INPUT:
##
##  occur.data ... filename for occurence data
##  absen.data  ... filename for absence data
##  enviro.data.current ... list of filenames for climate data
##  enviro.data.type    ... continuous
##  opt.tails ... predict parameter
##
##  outputdir ... root folder for output data

#define the working directory
#scriptdir = normalizePath(bccvl.params$scriptdir)
#inputdir =  normalizePath(bccvl.params$inputdir)
#outputdir =  normalizePath(bccvl.params$outputdir)


# extract params
# define the lon/lat of the observation records -- 2 column matrix of longitude and latitude
occur.data = bccvl.params$occurrence[1]
#define the the lon/lat of the background / psuedo absence points to use -- 2 column matrix of longitude and latitude
absen.data = bccvl.params$background[1]
#define the current enviro data to use
enviro.data.current = bccvl.params$environment
#type in terms of continuous or categorical
enviro.data.type = bccvl.params$environmenttype

#additional parameters for projecting bioclim
opt.tails = bccvl.params$tails # default "both"; use to ignore the left or right tail of the percentile distribution ("both", "low", "high"
opt.ext = NULL #an optional extent object to limit the prediction to a sub-region of 'x'
projection.name = "current"


# model accuracy statistics
# these are available from dismo::evaluate.R NOT originally implemented in biomod2::Evaluate.models.R
dismo.eval.method = c("ODP", "TNR", "FPR", "FNR", "NPP", "MCR", "OR")
# and vice versa
biomod.models.eval.meth = c("KAPPA", "TSS", "ROC", "FAR", "SR", "ACCURACY", "BIAS", "POD", "CSI", "ETS")
# model accuracy statistics - combine stats from dismo and biomod2 for consistent output
model.accuracy = c(dismo.eval.method, biomod.models.eval.meth)
# TODO: these functions are used to evaluate the model ... configurable?

# read current climate data
current.climate.scenario = bccvl.enviro.stack(enviro.data.current)

###read in the necessary observation, background and environmental data
occur = bccvl.species.read(occur.data) #read in the observation data lon/lat
# keep only lon and lat columns
occur = occur[c("lon","lat")]

# prepare absence points
if (bccvl.params$pseudoabsences$enabled) {
    # generate randomPoints
    bkgd = randomPoints(
        current.climate.scenario,
        bccvl.params$pseudoabsences$points,
        occur)
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
# TODO: combine random and given absence points:
# rbind(absen.datafromfile, bkgd.datarandom)

# extract enviro data for species observation points and append to species data
occur = cbind(occur, extract(current.climate.scenario, cbind(occur$lon, occur$lat)))
if (!is.null(absen)) {
    absen = cbind(absen, extract(current.climate.scenario, cbind(absen$lon, absen$lat)))
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
    model.sdm = bioclim(x=occur[,names(current.climate.scenario)])
    # save out the model object
    bccvl.save(model.sdm, paste(bccvl.params$species, "model.object.RData", sep="."))
    # predict for given climate scenario
    model.proj = predict(model.sdm, current.climate.scenario, tails=opt.tails)
    # save output
    bccvl.saveModelProjection(model.proj, projection.name)
    # evaluate model
    if (!is.null(absen)) {
        bccvl.evaluate.model('bioclim', model.sdm, occur, absen)
    }
} # end if continuous
