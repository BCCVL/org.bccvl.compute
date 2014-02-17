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

brt.fold.vector = NULL #a fold vector to be read in for cross validation with offsets
brt.tree.complexity = bccvl.params$tree_complexity #sets the complexity of individual trees
brt.learning.rate = bccvl.params$learning_rate #sets the weight applied to individual trees
brt.bag.fraction = bccvl.params$bag_fraction #sets the proportion of observations used in selecting variables
#brt.site.weights = rep(1, nrow(data)) #allows varying weighting for sites
#brt.var.monotone = rep(0, length(gbm.x)) #restricts responses to individual predictors to monotone
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
brt.keep.fold.vector = FALSE #Logical. allows the vector defining fold membership to be kept
brt.keep.fold.fit = FALSE #Logical. allows the predicted values for observations from cross-validation to be kept
projection.name = "current"

# model accuracy statistics
# these are available from dismo::evaluate.R NOT originally implemented in biomod2::Evaluate.models.R
dismo.eval.method = c("ODP", "TNR", "FPR", "FNR", "NPP", "MCR", "OR")
# and vice versa
biomod.models.eval.meth = c("KAPPA", "TSS", "ROC", "FAR", "SR", "ACCURACY", "BIAS", "POD", "CSI", "ETS")

# model accuracy statistics - combine stats from dismo and biomod2 for consistent output
model.accuracy = c(dismo.eval.method, biomod.models.eval.meth)

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



brt.data = rbind(occur,bkgd)
# setup the data as needed
brt.data$pa = c(rep(1,nrow(occur)),rep(0,nrow(bkgd)))
# run the algorithm
brt <- gbm.step(
        data=brt.data,
        gbm.x=which(names(brt.data) %in% names(current.climate.scenario)),
        gbm.y=which(names(brt.data)=='pa'),
        fold.vector = brt.fold.vector,
        tree.complexity = brt.tree.complexity,
        learning.rate = brt.learning.rate,
        bag.fraction = brt.bag.fraction,
        #site.weights = brt.site.weights,
        #var.monotone = brt.var.monotone,
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
bccvl.save(brt, "model.object.RData")
# NOTE the order of arguments in the predict function for brt; this is because
# the function is defined outside of the dismo package
# predict for CURRENT climate scenario
brt.proj = predict(current.climate.scenario, brt, n.trees=brt$gbm.call$best.trees, type="response")
bccvl.saveModelProjection(brt.proj, projection.name)
# evaluate model
bccvl.evaluate.model('brt', brt, occur, bkgd)
