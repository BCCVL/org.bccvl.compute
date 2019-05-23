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

############### BIOMOD2 Models ###############
#
# general parameters to perform any biomod modelling
#
biomod.NbRunEval = bccvl.params$nb_run_eval  # default 10; n-fold cross-validation; ignored if DataSplitTable is filled
biomod.DataSplit = bccvl.params$data_split # default 100; % for calibrating/training, remainder for testing; ignored if DataSplitTable is filled
biomod.Yweights = NULL #response points weights
biomod.Prevalence = bccvl.params$prevalence #either NULL (default) or a 0-1 numeric used to build "weighted response weights"
biomod.VarImport = bccvl.params$var_import # default 0; number of resampling of each explanatory variable to measure the relative importance of each variable for each selected model
#EMG this parameter needs to be specified in order to get VariableImportance metrics during model evaluation
biomod.models.eval.meth = c("KAPPA", "TSS", "ROC" ,"FAR", "SR", "ACCURACY", "BIAS", "POD", "CSI", "ETS") #vector of evaluation metrics
biomod.rescal.all.models = bccvl.params$rescale_all_models #if true, all model prediction will be scaled with a binomial GLM
biomod.do.full.models = bccvl.params$do_full_models #if true, models calibrated and evaluated with the whole dataset are done; ignored if DataSplitTable is filled
biomod.modeling.id = bccvl.params$modeling_id  #character, the ID (=name) of modeling procedure. A random number by default
# biomod.DataSplitTable = NULL #a matrix, data.frame or a 3D array filled with TRUE/FALSE to specify which part of data must be used for models calibration (TRUE) and for models validation (FALSE). Each column correspund to a "RUN". If filled, args NbRunEval, DataSplit and do.full.models will be ignored
# EMG Need to test whether a NULL values counts as an argument
biomod.species.name = occur.species # used for various path and file name generation
projection.name = "current"  #basename(enviro.data.current)
species_algo_str = ifelse(is.null(bccvl.params$subset), 
                          sprintf("%s_gbm", occur.species), 
                          sprintf("%s_gbm_%s", occur.species, bccvl.params$subset))


# model-specific arguments to create a biomod model
model.options.gbm <- list(
    distribution = bccvl.params$distribution, # "bernoulli", "gaussian", "laplace", "tdist", "huberized", "multinomial", "adaboost", "poisson", "coxph", "quantile", or "pairwise"
    n.trees = bccvl.params$n_trees, # The total number of trees to fit
    interaction.depth = bccvl.params$interaction_depth, # Maximum depth of variable interactions
    n.minobsinnode = bccvl.params$n_minobsinnode,
    shrinkage = bccvl.params$shrinkage, # A shrinkage parameter applied to each tree in the expansion
    bag.fraction = bccvl.params$bag_fraction, # The fraction of the training set observations randomly selected to propose the next tree in the expansion
    train.fraction = bccvl.params$train_fraction, # The first train.fraction * nrows(data) observations are used to fit the gbm
    cv.folds = bccvl.params$cv_folds # Number of cross-validation folds to perform
)

############### BIOMOD2 Models ###############
#
# general parameters to project any biomod modelling
#
#modeling.output #"BIOMOD.models.out" object produced by a BIOMOD_Modeling run
#new.env #a set of explanatory variables onto which models will be projected; must match variable names used to build the models
#proj.name #a character defining the projection name (a new folder will be created with this name)
# pseudo absences
biomod.PA.nb.rep = 0
biomod.PA.nb.absences = 0

biomod.xy.new.env = NULL #optional coordinates of new.env data. Ignored if new.env is a rasterStack
biomod.selected.models = bccvl.params$selected_models #'all' when all models have to be used to render projections or a subset vector of modeling.output models computed (eg, = grep('_RF', getModelsBuiltModels(myBiomodModelOut)))
# EMG If running one model at a time, this parameter becomes irrevelant
biomod.binary.meth = NULL #a vector of a subset of models evaluation method computed in model creation
biomod.filtered.meth = NULL #a vector of a subset of models evaluation method computed in model creation
biomod.compress = bccvl.params$compress # default 'gzip'; compression format of objects stored on your hard drive. May be one of `xz', `gzip' or NULL
biomod.build.clamping.mask = FALSE #if TRUE, a clamping mask will be saved on hard drive
opt.biomod.silent = FALSE #logical, if TRUE, console outputs are turned off
opt.biomod.do.stack = TRUE #logical, if TRUE, attempt to save all projections in a unique object i.e RasterStack
opt.biomod.keep.in.memory = TRUE #logical, if FALSE only the link pointing to a hard drive copy of projections are stored in output object
opt.biomod.output.format = NULL #'.Rdata', '.grd' or '.img'; if NULL, and new.env is not a Raster class, output is .RData defining projections saving format (on hard drive)


# model accuracy statistics
# these are available from dismo::evaluate.R NOT originally implemented in biomod2::Evaluate.models.R
dismo.eval.method = c("ODP", "TNR", "FPR", "FNR", "NPP", "MCR", "OR")

# model accuracy statistics - combine stats from dismo and biomod2 for consistent output
model.accuracy = c(dismo.eval.method, biomod.models.eval.meth)
# TODO: these functions are used to evaluate the model ... configurable?

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


###run the models and store models
############### BIOMOD2 Models ###############
# 1. Format the data
# 2. Define the model options
# 3. Compute the model
# NOTE: Model evaluation is included as part of model creation

# BIOMOD_Modeling(data, models = c('GLM','GBM','GAM','CTA','ANN','SRE','FDA','MARS','RF','MAXENT'), models.options = NULL,
#   NbRunEval=1, DataSplit=100, Yweights=NULL, Prevalence=NULL, VarImport=0, models.eval.meth = c('KAPPA','TSS','ROC'),
#   SaveObj = TRUE, rescal.all.models = TRUE, do.full.models = TRUE, modeling.id = as.character(format(Sys.time(), '%s')),
#   ...)
#
# data  BIOMOD.formated.data object returned by BIOMOD_FormatingData
# models vector of models names choosen among 'GLM', 'GBM', 'GAM', 'CTA', 'ANN', 'SRE', 'FDA', 'MARS', 'RF' and 'MAXENT'
# models.options BIOMOD.models.options object returned by BIOMOD_ModelingOptions
# NbRunEval Number of Evaluation run
# DataSplit % of data used to calibrate the models, the remaining part will be used for testing
# Yweights response points weights
# Prevalence either NULL (default) or a 0-1 numeric used to build 'weighted response weights'
# VarImport Number of permutation to estimate variable importance
# models.eval.meth vector of names of evaluation metric among 'KAPPA', 'TSS', 'ROC', 'FAR', 'SR', 'ACCURACY', 'BIAS', 'POD', 'CSI' and 'ETS'
# SaveObj keep all results and outputs on hard drive or not (NOTE: strongly recommended)
# rescal.all.models if true, all model prediction will be scaled with a binomial GLM
# do.full.models if true, models calibrated and evaluated with the whole dataset are done
# modeling.id character, the ID (=name) of modeling procedure. A random number by default.
# ... further arguments :
# DataSplitTable : a matrix, data.frame or a 3D array filled with TRUE/FALSE to specify which part of data must be used for models calibration (TRUE) and for models validation (FALSE). Each column correspund to a 'RUN'. If filled, args NbRunEval, DataSplit and do.full.models will be ignored.


###############
#
# GBM - generalized boosting model (usually called boosted regression trees) (gbm)
#
###############

# myBiomodOptions <- BIOMOD_ModelingOptions(GBM = list(distribution = 'bernoulli', interaction.depth = 7,
#   shrinkage = 0.001, bag.fraction = 0.5, train.fraction = 1, n.trees = 500, cv.folds = 5))
# n.trees : the total number of trees to fit. This is equivalent to the number of iterations and the number of basis functions in the additive expansion.
# cv.folds : Number of cross-validation folds to perform. If cv.folds>1 then gbm, in addition to the usual fit, will perform a cross-validation, calculate an estimate of generalization error returned in cv.error.
# distribution : either a character string specifying the name of the distribution to use or a list with a component name specifying the distribution and any additional parameters needed.
#   If not specified, gbm will try to guess: if the response has only 2 unique values, bernoulli is assumed; otherwise, if the response is a factor, multinomial is assumed; otherwise, if the response has class "Surv", coxph is assumed; otherwise, gaussian is assumed.
#   Currently available options are "gaussian" (squared error), "laplace" (absolute loss), "tdist" (t-distribution loss), "bernoulli" (logistic regression for 0-1 outcomes), "huberized" (huberized hinge loss for 0-1 outcomes), "multinomial" (classification when there are more than 2 classes), "adaboost" (the AdaBoost exponential loss for 0-1 outcomes), "poisson" (count outcomes), "coxph" (right censored observations), "quantile", or "pairwise" (ranking measure using the LambdaMart algorithm).
#   If quantile regression is specified, distribution must be a list of the form list(name="quantile",alpha=0.25) where alpha is the quantile to estimate. The current version's quantile regression method does not handle non-constant weights and will stop.
#   If "tdist" is specified, the default degrees of freedom is 4 and this can be controlled by specifying distribution=list(name="tdist", df=DF) where DF is your chosen degrees of freedom.
#   If "pairwise" regression is specified, distribution must be a list of the form list(name="pairwise",group=...,metric=...,max.rank=...) (metric and max.rank are optional, see below).
#       group is a character vector with the column names of data that jointly indicate the group an instance belongs to (typically a query in Information Retrieval applications). For training, only pairs of instances from the same group and with different target labels can be considered.
#       metric is the IR measure to use, one of:
#           conc - Fraction of concordant pairs; for binary labels, this is equivalent to the Area under the ROC Curve
#           mrr - Mean reciprocal rank of the highest-ranked positive instance
#           map - Mean average precision, a generalization of mrr to multiple positive instances
#           ndcg - Normalized discounted cumulative gain. The score is the weighted sum (DCG) of the user-supplied target values, weighted by log(rank+1), and normalized to the maximum achievable value. This is the default if the user did not specify a metric.
#               ndcg and conc allow arbitrary target values, while binary targets {0,1} are expected for conc and ndcg.
#       For ndcg and mrr, a cut-off can be chosen using a positive integer parameter max.rank. If left unspecified, all ranks are taken into account.
#   Note that splitting of instances into training and validation sets follows group boundaries and therefore only approximates the specified train.fraction ratio (the same applies to cross-validation folds). Internally, queries are randomly shuffled before training, to avoid bias.
#   Weights can be used in conjunction with pairwise metrics, however it is assumed that they are constant for instances from the same group.
#   For details and background on the algorithm, see e.g. Burges (2010).
# interaction.depth : The maximum depth of variable interactions. 1 implies an additive model, 2 implies a model with up to 2-way interactions, etc.
# shrinkage : a shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction.
# bag.fraction : the fraction of the training set observations randomly selected to propose the next tree in the expansion. This introduces randomnesses into the model fit.
#   If bag.fraction<1 then running the same model twice will result in similar but different fits. gbm uses the R random number generator so set.seed can ensure that the model can be reconstructed.
#   Preferably, the user can save the returned gbm.object using save.
# train.fraction : The first train.fraction * nrows(data) observations are used to fit the gbm and the remainder are used for computing out-of-sample estimates of the loss function.

# 1. Format the data as required by the biomod package
model.data = bccvl.biomod2.formatData(true.absen         = absen,
                                  pseudo.absen.points    = pa_number_point,
                                  pseudo.absen.strategy  = bccvl.params$pa_strategy,
                                  pseudo.absen.disk.min  = bccvl.params$pa_disk_min,
                                  pseudo.absen.disk.max  = bccvl.params$pa_disk_max,
                                  pseudo.absen.sre.quant = bccvl.params$pa_sre_quant,
                                  climate.data           = current.climate.scenario,
                                  occur                  = occur,
                                  species.name           = biomod.species.name,
                                  species_algo_str       = species_algo_str)

# 2. Define the model options
model.options <- BIOMOD_ModelingOptions(GBM = model.options.gbm)
# 3. Compute the model
model.sdm <-
    BIOMOD_Modeling(data = model.data,
                    models=c('GBM'),
                    models.options=model.options,
                    NbRunEval=biomod.NbRunEval,
                    DataSplit=biomod.DataSplit,
                    Yweights=biomod.Yweights,
                    Prevalence=biomod.Prevalence,
                    VarImport=biomod.VarImport,
                    models.eval.meth=biomod.models.eval.meth,
                    SaveObj=TRUE,
                    rescal.all.models = biomod.rescal.all.models,
                    do.full.models = biomod.do.full.models,
                    modeling.id = biomod.modeling.id
                    )

# save the VIP plot
x.data <- attr(model.data,"data.env.var")
y.data <- attr(model.data,"data.species")
data1 = data.frame(y.data,x.data)
bccvl.VIPplot(method="gbm", data1=data1, pdf=TRUE, 
              filename=paste('vip_plot', species_algo_str, sep="_"), 
              this.dir=paste(biomod.species.name, "/models/bccvl", sep=""))

# model output saved as part of BIOMOD_Modeling() # EMG not sure how to retrieve
#save out the model object
bccvl.save(model.sdm, name="model.object.RData")

# Do projection over current climate scenario without constraint only if all env data layers are continuous.
if (enviro.data.genUnconstraintMap &&
   all(enviro.data.type == 'continuous') && 
   (!is.null(enviro.data.constraints) || enviro.data.generateCHall)) {
    model.proj <-
        BIOMOD_Projection(modeling.output     = model.sdm,
                          new.env             = current.climate.scenario.orig,
                          proj.name           = projection.name,
                          xy.new.env          = biomod.xy.new.env,
                          selected.models     = biomod.selected.models,
                          binary.meth         = biomod.binary.meth,
                          filtered.meth       = biomod.filtered.meth,
                          #compress            = biomod.compress,
                          build.clamping.mask = biomod.build.clamping.mask,
                          silent              = opt.biomod.silent,
                          do.stack            = opt.biomod.do.stack,
                          keep.in.memory      = opt.biomod.keep.in.memory,
                          output.format       = opt.biomod.output.format,
                          on_0_1000           = FALSE)

    # remove the current.climate.scenario to release disk space
    bccvl.remove.rasterObject(current.climate.scenario.orig)

    # convert projection output from grd to gtiff
    bccvl.grdtogtiff(file.path(getwd(),
                               biomod.species.name,
                               paste("proj", projection.name, sep="_")), 
                     algorithm=ifelse(is.null(bccvl.params$subset), "gbm", sprintf("gbm_%s", bccvl.params$subset)), 
                     filename_ext="unconstrained")

    # save the projection
    bccvl.saveProjection(model.proj, species_algo_str, filename_ext="unconstrained")
}

# predict for current climate scenario
model.proj <-
    BIOMOD_Projection(modeling.output=model.sdm,
                      new.env=current.climate.scenario,
                      proj.name  = projection.name,  #basename(enviro.data.current), {{ species }}
                      xy.new.env = biomod.xy.new.env,
                      selected.models = biomod.selected.models,
                      binary.meth = biomod.binary.meth,
                      filtered.meth = biomod.filtered.meth,
                      #compress = biomod.compress,
                      build.clamping.mask = biomod.build.clamping.mask,
                      silent = opt.biomod.silent,
                      do.stack = opt.biomod.do.stack,
                      keep.in.memory = opt.biomod.keep.in.memory,
                      output.format = opt.biomod.output.format,
                      on_0_1000 = FALSE)

# remove the current.climate.scenario to release disk space
bccvl.remove.rasterObject(current.climate.scenario)

# convert projection output from grd to gtiff
bccvl.grdtogtiff(file.path(getwd(),
                           biomod.species.name,
                           paste("proj", projection.name, sep="_")),
                 algorithm=ifelse(is.null(bccvl.params$subset), "gbm", sprintf("gbm_%s", bccvl.params$subset)))


# output is saved as part of the projection, format specified in arg 'opt.biomod.output.format'
loaded.model = BIOMOD_LoadModels(model.sdm, models="GBM")
bccvl.saveBIOMODModelEvaluation(loaded.model, model.sdm, species_algo_str)    # save output

# save the projection
bccvl.saveProjection(model.proj, species_algo_str)
