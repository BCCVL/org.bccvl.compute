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
#scriptdir = normalizePath(bccvl.env$scriptdir)
#inputdir =  normalizePath(bccvl.env$inputdir)
#outputdir =  normalizePath(bccvl.env$outputdir)


# extract params
# define the lon/lat of the observation records -- 2 column matrix of longitude and latitude
occur.data = bccvl.params$species_occurrence_dataset$filename
occur.species = bccvl.params$species_occurrence_dataset$species
month.filter = bccvl.params$species_filter
#define the the lon/lat of the background / psuedo absence points to use -- 2 column matrix of longitude and latitude
absen.data = bccvl.params$species_absence_dataset$filename
#define the current enviro data to use
enviro.data.current = lapply(bccvl.params$environmental_datasets, function(x) x$filename)
#type in terms of continuous or categorical
enviro.data.type = lapply(bccvl.params$environmental_datasets, function(x) x$type)
#layer names for the current environmental layers used
enviro.data.layer = lapply(bccvl.params$environmental_datasets, function(x) x$layer)
#geographic constraints.
enviro.data.constraints = readLines(bccvl.params$modelling_region$filename)
#Indicate to generate and apply convex-hull polygon of occurrence dataset to constraint
enviro.data.generateCHall = ifelse(is.null(bccvl.params$generate_convexhull), FALSE, as.logical(bccvl.params$generate_convexhull))
#Indicate whether to generate unconstraint map or not. True by default
enviro.data.genUnconstraintMap = ifelse(is.null(bccvl.params$unconstraint_map), TRUE, as.logical(bccvl.params$unconstraint_map))
# resampling (up / down scaling) if scale_down is TRUE, return 'lowest'
enviro.data.resampling = ifelse(is.null(bccvl.params$scale_down) ||
                                as.logical(bccvl.params$scale_down),
                                'highest', 'lowest')

brt.fold.vector = NULL #a fold vector to be read in for cross validation with offsets
brt.tree.complexity = bccvl.params$tree_complexity #sets the complexity of individual trees
brt.learning.rate = bccvl.params$learning_rate #sets the weight applied to individual trees
brt.bag.fraction = bccvl.params$bag_fraction #sets the proportion of observations used in selecting variables
brt.site.weights = NULL # rep(1, nrow(data)) #allows varying weighting for sites
brt.var.monotone = NULL # rep(0, length(gbm.x)) #restricts responses to individual predictors to monotone
brt.n.folds = bccvl.params$n_folds #number of folds
brt.prev.stratify = bccvl.params$prev_stratify #prevalence stratify the folds - only for presence/absence data
brt.family = bccvl.params$family #family - bernoulli (=binomial), poisson, laplace or gaussian
brt.n.trees = bccvl.params$n_trees #number of initial trees to fit
brt.step.size = brt.n.trees #numbers of trees to add at each cycle
brt.max.trees = bccvl.params$max_trees #max number of trees to fit before stopping
brt.tolerance.method = bccvl.params$tolerance_method #method to use in deciding to stop - "fixed" or "auto"
brt.tolerance = bccvl.params$tolerance_value #tolerance value to use - if method == fixed is absolute, if auto is multiplier * total mean deviance
brt.keep.data = FALSE #Logical. keep raw data in final model
brt.plot.main = FALSE #Logical. plot hold-out deviance curve
brt.plot.folds = FALSE #Logical. plot the individual folds as well
brt.verbose = FALSE #Logical. control amount of screen reporting
brt.silent = FALSE #Logical. to allow running with no output for simplifying model)
brt.keep.fold.models = FALSE #Logical. keep the fold models from cross valiation
brt.keep.fold.vector = TRUE #Logical. allows the vector defining fold membership to be kept
brt.keep.fold.fit = FALSE #Logical. allows the predicted values for observations from cross-validation to be kept
projection.name = "current"
species_algo_str = ifelse(is.null(bccvl.params$subset), 
                          sprintf("%s_brt", occur.species), 
                          sprintf("%s_brt_%s", occur.species, bccvl.params$subset))


# model accuracy statistics
# these are available from dismo::evaluate.R NOT originally implemented in biomod2::Evaluate.models.R
dismo.eval.method = c("ODP", "TNR", "FPR", "FNR", "NPP", "MCR", "OR")
# and vice versa
biomod.models.eval.meth = c("KAPPA", "TSS", "ROC", "FAR", "SR", "ACCURACY", "BIAS", "POD", "CSI", "ETS")

# model accuracy statistics - combine stats from dismo and biomod2 for consistent output
model.accuracy = c(dismo.eval.method, biomod.models.eval.meth)

# read current climate data
current.climate.scenario = bccvl.enviro.stack(enviro.data.current, enviro.data.type, enviro.data.layer, resamplingflag=enviro.data.resampling)

###read in the necessary observation, background and environmental data
occur = bccvl.species.read(occur.data, month.filter) #read in the observation data lon/lat
absen = bccvl.species.read(absen.data, month.filter) #read in the observation data lon/lat

# geographically constrained modelling
if (!is.null(enviro.data.constraints) || enviro.data.generateCHall) {
  constrainedResults = bccvl.sdm.geoconstrained(current.climate.scenario, occur, absen, enviro.data.constraints, enviro.data.generateCHall);
  
  # Save a copy of the climate dataset
  current.climate.scenario.orig <- current.climate.scenario  
  current.climate.scenario <- constrainedResults$raster
  occur <- constrainedResults$occur
  absen <- constrainedResults$absen
}

# Determine the number of pseudo absence points from pa_ratio
pa_ratio = bccvl.params$pa_ratio
pa_number_point = 0
if (pa_ratio > 0) {
  pa_number_point = floor(pa_ratio * nrow(occur))
}


#############################################################################################
#
# MACHINE LEARNING METHODS - use both presence and absence or background data: Maxent, BRT
#
#############################################################################################

###############
#
# BRT
#
###############

# gbm.step(data, gbm.x, gbm.y, offset = NULL, fold.vector = NULL, tree.complexity = 1,
# learning.rate = 0.01, bag.fraction = 0.75, site.weights = rep(1, nrow(data)),
# var.monotone = rep(0, length(gbm.x)), n.folds = 10, prev.stratify = TRUE,
# family = "bernoulli", n.trees = 50, step.size = n.trees, max.trees = 10000,
# tolerance.method = "auto", tolerance = 0.001, keep.data = FALSE, plot.main = TRUE,
# plot.folds = FALSE, verbose = TRUE, silent = FALSE, keep.fold.models = FALSE,
# keep.fold.vector = FALSE, keep.fold.fit = FALSE, ...)
# data input data.frame
# gbm.x predictor variables
# gbm.y response variable
# offset = NULL
# fold.vector = NULL a fold vector to be read in for cross validation with offsets
# tree.complexity = 1 sets the complexity of individual trees
# learning.rate = 0.01 sets the weight applied to individual trees
# bag.fraction = 0.75 sets the proportion of observations used in selecting variables
# site.weights = rep(1, nrow(data)) allows varying weighting for sites
# var.monotone = rep(0, length(gbm.x)) restricts responses to individual predictors to monotone
# n.folds = 10 number of folds
# prev.stratify = TRUE prevalence stratify the folds - only for presence/absence data
# family = "bernoulli" family - bernoulli (=binomial), poisson, laplace or gaussian
# n.trees = 50 number of initial trees to fit
# step.size = n.trees numbers of trees to add at each cycle
# max.trees = 10000 max number of trees to fit before stopping
# tolerance.method = "auto" method to use in deciding to stop - "fixed" or "auto"
# tolerance = 0.001 tolerance value to use - if method == fixed is absolute,
# if auto is multiplier * total mean deviance
# keep.data = FALSE Logical. keep raw data in final model
# plot.main = TRUE Logical. plot hold-out deviance curve
# plot.folds = FALSE Logical. plot the individual folds as well
# verbose = TRUE Logical. control amount of screen reporting
# silent = FALSE Logical. to allow running with no output for simplifying model)
# keep.fold.models = FALSE Logical. keep the fold models from cross valiation
# keep.fold.vector = FALSE Logical. allows the vector defining fold membership to be kept
# keep.fold.fit = FALSE Logical. allows the predicted values for observations from cross-validation
# to be kept


# Format the data as in biomod2. This will also generate the psedo absence points.
biomod2.data = bccvl.biomod2.formatData(true.absen       = absen,
                                  pseudo.absen.points    = pa_number_point,
                                  pseudo.absen.strategy  = bccvl.params$pa_strategy,
                                  pseudo.absen.disk.min  = bccvl.params$pa_disk_min,
                                  pseudo.absen.disk.max  = bccvl.params$pa_disk_max,
                                  pseudo.absen.sre.quant = bccvl.params$pa_sre_quant,
                                  climate.data           = current.climate.scenario,
                                  occur                  = occur,
                                  species.name           = occur.species,
                                  species_algo_str       = species_algo_str)

# Extract occurrence and absence data
coord = cbind(biomod2.data@coord, biomod2.data@data.env.var)
occur = coord[c(which(biomod2.data@data.species == 1)), names(coord)]
absen = coord[c(which(biomod2.data@data.species == 0 | is.na(biomod2.data@data.species))), names(coord)]


brt.data = coord
# setup the data as needed
brt.data$pa = c(rep(1,nrow(occur)),rep(0,nrow(absen)))
# run the algorithm
model.sdm <- gbm.step(
        data=brt.data,
        gbm.x=which(names(brt.data) %in% names(current.climate.scenario)),
        gbm.y=which(names(brt.data)=='pa'),
        fold.vector = brt.fold.vector,
        tree.complexity = brt.tree.complexity,
        learning.rate = brt.learning.rate,
        bag.fraction = brt.bag.fraction,
        site.weights = brt.site.weights,
        var.monotone = brt.var.monotone,
        n.folds = brt.n.folds,
        prev.stratify = brt.prev.stratify,
        family = brt.family,
        n.trees = brt.n.trees,
        step.size = brt.step.size,
        max.trees = brt.max.trees,
        tolerance.method = brt.tolerance.method,
        tolerance = brt.tolerance,
        keep.data = brt.keep.data,
        plot.main = brt.plot.main,
        plot.folds = brt.plot.folds,
        verbose = brt.verbose,
        silent = brt.silent,
        keep.fold.models = brt.keep.fold.models,
        keep.fold.vector = brt.keep.fold.vector,
        keep.fold.fit = brt.keep.fold.fit)

#save out the model object
bccvl.save(model.sdm, bccvl.format.outfilename(filename="model.object", id_str=species_algo_str, ext="RData"))

# NOTE the order of arguments in the predict function for brt; this is because
# the function is defined outside of the dismo package
# predict for CURRENT climate scenario

# Do projection over current climate scenario without constraint only if all env data layers are continuous.
if (enviro.data.genUnconstraintMap && 
   all(enviro.data.type == 'continuous') && 
   (!is.null(enviro.data.constraints) || enviro.data.generateCHall)) {
    model.proj = predict(current.climate.scenario.orig, model.sdm, n.trees=model.sdm$gbm.call$best.trees, type="response")

    # remove the current.climate.scenario to release disk space
    bccvl.remove.rasterObject(current.climate.scenario.orig)

    # save output
    bccvl.saveModelProjection(model.proj, projection.name, occur.species, species_algo_str, filename_ext="unconstrained")
}

model.proj = predict(current.climate.scenario, model.sdm, n.trees=model.sdm$gbm.call$best.trees, type="response")

# remove the current.climate.scenario to release disk space
bccvl.remove.rasterObject(current.climate.scenario)

bccvl.saveModelProjection(model.proj, projection.name, occur.species, species_algo_str)

# evaluate model
bccvl.saveDISMOModelEvaluation('brt', model.sdm, occur, absen, occur.species)
