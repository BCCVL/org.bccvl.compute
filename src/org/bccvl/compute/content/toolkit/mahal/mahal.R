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

#define the working environment
#scriptdir = normalizePath(bccvl.env$scriptdir)
#inputdir =  normalizePath(bccvl.env$inputdir)
#outputdir =  normalizePath(bccvl.env$outputdir)


# extract params
# define the lon/lat of the observation records -- 2 column matrix of longitude and latitude
occur.data = bccvl.params$species_occurrence_dataset$filename
occur.species = bccvl.params$species_occurrence_dataset$species
#define the the lon/lat of the background / psuedo absence points to use -- 2 column matrix of longitude and latitude
absen.data = bccvl.params$species_absence_dataset$filename
#define the current enviro data to use
enviro.data.current = lapply(bccvl.params$environmental_datasets, function(x) x$filename)
#type in terms of continuous or categorical
enviro.data.type = lapply(bccvl.params$environmental_datasets, function(x) x$type)
#geographic constraints
enviro.data.constraints = bccvl.params$modelling_region
# resampling (up / down scaling) if scale_down is TRUE, return 'lowest'
enviro.data.resampling = ifelse(is.null(bccvl.params$scale_down) ||
                                ! as.logical(bccvl.params$scale_down),
                                'highest', 'lowest')

#additional parameters for projecting circles
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
current.climate.scenario = bccvl.enviro.stack(enviro.data.current, enviro.data.type, resamplingflag=enviro.data.resampling)

###read in the necessary observation, background and environmental data
occur = bccvl.species.read(occur.data) #read in the observation data lon/lat
# keep only lon and lat columns
occur = occur[c("lon","lat")]

# geographically constrained modelling
if (!is.null(enviro.data.constraints)) {
  constrainedResults = bccvl.sdm.geoconstrained(current.climate.scenario, occur, enviro.data.constraints);
  
  current.climate.scenario <- constrainedResults$raster
  occur <- constrainedResults$occur
}

# Determine the number of pseudo absence points from pa_ratio
pa_ratio = bccvl.params$pa_ratio
pa_number_point = 0
if (pa_ratio > 0) {
  pa_number_point = floor(pa_ratio * nrow(occur))
}

# Format the data as in biomod2. This will also generate the psedo absence points.
biomod2.data = bccvl.biomod2.formatData(absen.filename   = absen.data,
                                  pseudo.absen.points    = pa_number_point,
                                  pseudo.absen.strategy  = bccvl.params$pa_strategy,
                                  pseudo.absen.disk.min  = bccvl.params$pa_disk_min,
                                  pseudo.absen.disk.max  = bccvl.params$pa_disk_max,
                                  pseudo.absen.sre.quant = bccvl.params$pa_sre_quant,
                                  climate.data           = current.climate.scenario,
                                  occur                  = occur,
                                  species.name           = occur.species)

# Extract occurrence and absence data
coord = cbind(biomod2.data@coord, biomod2.data@data.env.var)
occur = coord[c(which(biomod2.data@data.species == 1)), names(coord)]
absen = coord[c(which(biomod2.data@data.species == 0 | is.na(biomod2.data@data.species))), names(coord)]


###############
#
# MAHALANOBIS
#
###############

# mahal(x, p, ...)
# x is a Raster* object or matrix
# p is a two column matrix or SpatialPoints* object
# if p is missing, x is a matrix of values of env vars at known locations of occurrence
# if p is present, it is the location of occurrence and used to extract values for env vars from x,
#	a Raster* object
# NOTE: env vars must be numerical


if (!all(enviro.data.type=="continuous")) {
    stop("mahal does not support categorical feature data")
} else {
    # run mahal with matrix of enviro data
    model.sdm = mahal(x=current.climate.scenario, p=occur[,c("lon", "lat")], error = bccvl.err.null)
    # save out the model object
    bccvl.save(model.sdm, paste(occur.species, "model.object.RData", sep="."))
    # predict for given climate scenario
    model.proj = predict(model.sdm, current.climate.scenario, tails=opt.tails)
    # save output
    bccvl.saveModelProjection(model.proj, projection.name, occur.species)
    # evaluate model
    if (!is.null(absen)) {
        bccvl.evaluate.model('mahal', model.sdm, occur, absen)
    }
} # end if continuous
