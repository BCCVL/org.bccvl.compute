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

#define the working directory
#scriptdir = normalizePath(bccvl.params$scriptdir)
#inputdir =  normalizePath(bccvl.params$inputdir)
#outputdir =  normalizePath(bccvl.params$outputdir)


# extract params
# define the lon/lat of the observation records -- 2 column matrix of longitude and latitude
occur.data = bccvl.params$occurrence[1]
#define the the lon/lat of the background / psuedo absence points to use -- 2 column matrix of longitude and latitude
bkgd.data = bccvl.params$background[1]
#define the current enviro data to use
enviro.data.current = bccvl.params$environment
#type in terms of continuous or categorical
enviro.data.type = bccvl.params$environmenttype

#additional parameters for projecting convHull
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
current.climate.scenario = stack(enviro.data.current)

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
    bkgd = as.data.frame(bkgd)
    # rename columns
    names(bkgd) <- c("lon","lat")
} else {
    # otherwise read absence ponits from file
    bkgd = bccvl.species.read(bkgd.data) #read in the background position data lon.lat
    # keep only lon and lat columns
    bkgd = bkgd[c("lon","lat")]
}
# TODO: combine random and given absence points:
# rbind(bkgd.datafromfile, bkgd.datarandom)

# extract enviro data for species observation points and append to species data
occur = cbind(occur, extract(current.climate.scenario, cbind(occur$lon, occur$lat)))
if (!is.null(bkgd)) {
    bkgd = cbind(bkgd, extract(current.climate.scenario, cbind(bkgd$lon, bkgd$lat)))
}


###############
#
# CONVEX HULL
#
###############

# convHull(p, ...)
# p point locations (presence), two column matrix, data.frame or SpatialPoints* object
# ... you can supply an argument n (>=1) to get n convex hulls around subset of the points
# ... you can also set n=1:x, to get a set of overlapping polygons consisting of 1 to x parts; i.e.
#   the first polygon has 1 part, the second has 2 parts and x has x parts

if (!all(enviro.data.type=="continuous")) {
    warning("convhull not run because categorical data cannot be used")
} else {
    # run convhull with matrix of enviro data
    ch = convHull(p=occur)
    # save out the model object
    bccvl.save(ch, "model.object.RData")
    # predict for given climate scenario
    convhull.proj = predict(ch, current.climate.scenario, tails=opt.tails)
    # save output
    bccvl.saveModelProjection(convhull.proj, projection.name)
    # evaluate model
    if (!is.null(bkgd)) {
        bccvl.evaluate.model('convHull', ch, occur, bkgd)
    }
} # end if continuous
