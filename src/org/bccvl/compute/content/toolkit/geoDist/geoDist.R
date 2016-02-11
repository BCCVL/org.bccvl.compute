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
current.climate.scenario = bccvl.enviro.stack(enviro.data.current, resamplingflag="lowest")

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


# Format the data as in biomod2. This will also generate the psedo absence points.
biomod2.data = bccvl.biomod2.formatData(absen.filename  = absen.data,
                                  pseudo.absen.enabled  = bccvl.params$species_pseudo_absence_points,
                                  pseudo.absen.points   = bccvl.params$species_number_pseudo_absence_points,
                                  pseudo.absen.strategy = 'random',
                                  climate.data          = current.climate.scenario,
                                  occur                 = occur,
                                  species.name          = occur.species)

# Extract occurrence and absence data
coord = biomod2.data@coord
occur = coord[c(which(biomod2.data@data.species == 1)), names(coord)]
absen = coord[c(which(biomod2.data@data.species == 0 | is.na(biomod2.data@data.species))), names(coord)]


###############
#
# GEOGRAPHIC DISTANCE
#
###############

# geoDist(p, ...)
# p point locations (presence); two column matrix, data.frame or SpatialPoints* object
# ... you must supply a lonlat= argument(logical), unless p is a SpatialPoints* object and has a
#	valid CRS
# ... you can also supply an additional argument 'a' for absence points (currently ignored.); 
#	argument 'a' should be of the same class as argument 'p'

model.sdm = geoDist(p=occur, lonlat=TRUE)
# save out the model object
bccvl.save(model.sdm, paste(occur.species, "model.object.RData", sep="."))
# predict for given climate scenario
model.proj = predict(model.sdm, current.climate.scenario, tails=opt.tails)
# save output
bccvl.saveModelProjection(model.proj, projection.name, occur.species)

