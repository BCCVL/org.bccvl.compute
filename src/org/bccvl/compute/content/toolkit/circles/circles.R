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
#layer names for the current environmental layers used
enviro.data.layer = lapply(bccvl.params$environmental_datasets, function(x) x$layer)
#geographic constraints
enviro.data.constraints = bccvl.params$modelling_region
# resampling (up / down scaling) if scale_down is TRUE, return 'lowest'
enviro.data.resampling = ifelse(is.null(bccvl.params$scale_down) ||
                                as.logical(bccvl.params$scale_down),
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
current.climate.scenario = bccvl.enviro.stack(enviro.data.current, enviro.data.type, enviro.data.layer, resamplingflag=enviro.data.resampling)

###read in the necessary observation, background and environmental data
occur = bccvl.species.read(occur.data) #read in the observation data lon/lat
# keep only lon and lat columns
occur = occur[c("lon","lat")]

# geographically constrained modelling
if (!is.null(enviro.data.constraints)) {
  constrainedResults = bccvl.sdm.geoconstrained(current.climate.scenario, occur, enviro.data.constraints);

  # Save a copy of the climate dataset
  current.climate.scenario.orig <- current.climate.scenario  
  current.climate.scenario <- constrainedResults$raster
  occur <- constrainedResults$occur
}


# Circle algorithm does not need pseudo absence points
pa_number_point = 0

# Format the data as in biomod2. This will also generate the psedo absence points.
biomod2.data = bccvl.biomod2.formatData(absen.filename   = absen.data,
                                  pseudo.absen.points    = pa_number_point,
                                  pseudo.absen.strategy  = bccvl.params$pa_strategy,
                                  pseudo.absen.disk.min  = bccvl.params$pa_disk_min,
                                  pseudo.absen.disk.max  = bccvl.params$pa_disk_max,
                                  pseudo.absen.sre.quant = bccvl.params$pa_sre_quant,
                                  climate.data           = current.climate.scenario,
                                  occur                  = occur,
                                  species.name           = occur.species,
                                  save.pseudo.absen      = FALSE)
# Extract occurrence and absence data
coord = cbind(biomod2.data@coord, biomod2.data@data.env.var)
occur = coord[c(which(biomod2.data@data.species == 1)), names(coord)]

###############
#
# Circles
#
###############

# circles(p, ...)
# p point locations (presence), two column matrix, data.frame or SpatialPoints* object
# d the radius of each circle in meters; a single number or a vector with elements corresponding to
#   rows in 'p'; if missing the diameter is computed from the inter-point distance
# n how many vertices in the circle? default is 360
# lonlat are these longitude/latitude data? default value is false
# r radius of the earth; only relevant for longitude/latitude data; default is 6378137 m

if (!all(enviro.data.type=="continuous")) {
    warning("circles not run because categorical data cannot be used")
} else {
    # run circles with matrix of enviro data
    model.sdm = circles(p=occur, lonlat=TRUE)
    # save out the model object
    bccvl.save(model.sdm, paste(occur.species, "model.object.RData", sep="."))
    # predict for given climate scenario
    model.proj = predict(model.sdm, current.climate.scenario, tails=opt.tails)
    # save output
    bccvl.saveModelProjection(model.proj, projection.name, occur.species)

    # Do projection over current climate scenario without constraint
    if (!is.null(enviro.data.constraints)) {
        model.proj = predict(model.sdm, current.climate.scenario.orig, tails=opt.tails)
        # save output
        bccvl.saveModelProjection(model.proj, projection.name, occur.species, filename_ext="unconstraint")
    }
} # end if continuous
