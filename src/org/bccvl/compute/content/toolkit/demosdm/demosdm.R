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
#Indicate to generate and apply convex-hull polygon of occurrence dataset to constraint; 
# for demosdm, set to true as we want to use convex-hall polygon as the modelling region.
enviro.data.generateCHall = TRUE
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
species_algo_str = sprintf("%s_maxent", occur.species)

# model-specific arguments to create a biomod model
model.options.maxent <- list(
  path_to_maxent.jar = Sys.getenv("MAXENT"), #The path to maxent.jar file (the working directory by default)
  #memory_allocated = bccvl.params$memory_allocated #The amount of memory (in Mo) reserved for java to run MAXENT. should be 64, 128, 256, 512, 1024, 2048... or NULL if you want to use default java memory limitation parameter.
  maximumiterations = bccvl.params$maximumiterations,
  visible = FALSE,
  linear = bccvl.params$linear,
  quadratic = bccvl.params$quadratic,
  product = bccvl.params$product,
  threshold = bccvl.params$threshold,
  hinge = bccvl.params$hinge,
  lq2lqptthreshold = bccvl.params$lq2lqptthreshold,
  l2lqthreshold = bccvl.params$l2lqthreshold,
  hingethreshold = bccvl.params$hingethreshold,
  beta_threshold = bccvl.params$beta_threshold,
  beta_categorical = bccvl.params$beta_categorical,
  beta_lqp = bccvl.params$beta_lqp,
  beta_hinge = bccvl.params$beta_hinge,
  defaultprevalence = bccvl.params$defaultprevalence
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
#	NbRunEval=1, DataSplit=100, Yweights=NULL, Prevalence=NULL, VarImport=0, models.eval.meth = c('KAPPA','TSS','ROC'),
#	SaveObj = TRUE, rescal.all.models = TRUE, do.full.models = TRUE, modeling.id = as.character(format(Sys.time(), '%s')),
#	...)
#
# data	BIOMOD.formated.data object returned by BIOMOD_FormatingData
# models vector of models names choosen among 'GLM', 'GBM', 'GAM', 'CTA', 'ANN', 'SRE', 'FDA', 'MARS', 'RF' and 'MAXENT'
# models.options BIOMOD.models.options object returned by BIOMOD_ModelingOptions
# NbRunEval	Number of Evaluation run
# DataSplit	% of data used to calibrate the models, the remaining part will be used for testing
# Yweights response points weights
# Prevalence either NULL (default) or a 0-1 numeric used to build 'weighted response weights'
# VarImport	Number of permutation to estimate variable importance
# models.eval.meth vector of names of evaluation metric among 'KAPPA', 'TSS', 'ROC', 'FAR', 'SR', 'ACCURACY', 'BIAS', 'POD', 'CSI' and 'ETS'
# SaveObj keep all results and outputs on hard drive or not (NOTE: strongly recommended)
# rescal.all.models	if true, all model prediction will be scaled with a binomial GLM
# do.full.models if true, models calibrated and evaluated with the whole dataset are done
# modeling.id character, the ID (=name) of modeling procedure. A random number by default.
# ... further arguments :
# DataSplitTable : a matrix, data.frame or a 3D array filled with TRUE/FALSE to specify which part of data must be used for models calibration (TRUE) and for models validation (FALSE). Each column correspund to a 'RUN'. If filled, args NbRunEval, DataSplit and do.full.models will be ignored.

###############
#
# Maxent
#
###############

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
                                  generate.background.data = TRUE,                # Generate background data as pseudo absence data
                                  species_algo_str       = species_algo_str)

# 2. Define the model options
model.options <- BIOMOD_ModelingOptions(MAXENT = model.options.maxent)
# 3. Compute the model
model.sdm <-
    BIOMOD_Modeling(data = model.data,
                    models=c('MAXENT'),
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
# model output saved as part of BIOMOD_Modeling() # EMG not sure how to retrieve
#save out the model object
bccvl.save(model.sdm, name="model.object.RData")
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

# remove the environmental dataset to release disk space
bccvl.remove.rasterObject(current.climate.scenario)

# convert projection output from grd to gtiff
bccvl.grdtogtiff(file.path(getwd(),
                           biomod.species.name,
                           paste("proj", projection.name, sep="_")),
                 algorithm="maxent")


# output is saved as part of the projection, format specified in arg 'opt.biomod.output.format'
loaded.model = BIOMOD_LoadModels(model.sdm, models="MAXENT")
bccvl.saveBIOMODModelEvaluation(loaded.model, model.sdm, species_algo_str) 	# save output

# save the projection
bccvl.saveProjection(model.proj, biomod.species.name, species_algo_str)


#============================================================================================
# project the sdm model. This is from predict.R with modification to input parameters.
#============================================================================================
# Get the species name and sdm model from above
sdm.species = biomod.species.name
model.obj = model.sdm

projection.name = bccvl.params$cc_projection_name
#geographic constraints
enviro.data.constraints = bccvl.params$projection_region

# resampling (up / down scaling) if scale_down is TRUE, return 'lowest'
enviro.data.resampling = 'highest'

future.climate.dataset = lapply(bccvl.params$future_climate_datasets, function(x) x$filename)
future.climate.data.type = lapply(bccvl.params$future_climate_datasets, function(x) x$type)
#layer names for the current environmental layers used
future.climate.data.layer = lapply(bccvl.params$future_climate_datasets, function(x) x$layer)


projectdataset <- function(model.obj, futuredata, datatype, datalayername, projection.name, species) {
    future.climate.scenario = bccvl.enviro.stack(futuredata, datatype, datalayername, resamplingflag=enviro.data.resampling)
    # filter out unused layers from future.climate.scenario
    predictors <- bccvl.checkModelLayers(model.obj, future.climate.scenario, futuredata)
    # geographically constrained modelling
    if (!is.null(enviro.data.constraints || enviro.data.generateCHall)) {
      constrainedResults = bccvl.sdm.geoconstrained(predictors, NULL, NULL, enviro.data.constraints, enviro.data.generateCHall);
      predictors <- constrainedResults$raster
    }
    
    # do projection
    if (inherits(model.obj, "DistModel")) {
        # dismo package
        opt.tails <- bccvl.params$tails
        opt.ext <- NULL
        model.proj <- predict(model.obj,
                              predictors,
                              tails=opt.tails,
                              ext=opt.ext)

        # remove the environmental dataset to release disk space
        bccvl.remove.rasterObject(predictors)

        bccvl.saveModelProjection(model.proj, projection.name, species, species_algo_str)
    } else if (inherits(model.obj, "gbm")) {
        # brt package)
        model.proj <- predict(predictors,
                              model.obj,
                              n.trees=model.obj$gbm.call$best.trees,
                              type="response")
        # remove the environmental dataset to release disk space
        bccvl.remove.rasterObject(predictors)

        bccvl.saveModelProjection(model.proj, projection.name, species, species_algo_str)
    } else if (inherits(model.obj, "BIOMOD.models.out")) {
        # expect additional model data in input folder.
        # for biomod to find it we'll have to change wd

        biomod.xy.new.env <- NULL
        biomod.selected.models <- bccvl.params$selected_models
        biomod.binary.meth <- NULL
        biomod.filtered.meth <- NULL
        biomod.compress <- NULL # bccvl.params$compress
        biomod.build.clamping.mask <- TRUE
        biomod.species.name <-  species
        opt.biomod.silent <- FALSE
        opt.biomod.do.stack <- TRUE
        opt.biomod.keep.in.memory <- TRUE
        opt.biomod.output.format <- NULL

        model.proj <- BIOMOD_Projection(modeling.output=model.obj,
                                        new.env=predictors,
                                        proj.name=projection.name,
                                        xy.new.env=biomod.xy.new.env,
                                        selected.models=biomod.selected.models,
                                        binary.meth=biomod.binary.meth,
                                        filtered.meth=biomod.filtered.meth,
                                        # compress=biomod.compress, # .. Null not accepted
                                        build.clamping.mask=biomod.build.clamping.mask,
                                        silent=opt.biomod.silent,
                                        do.stack=opt.biomod.do.stack,
                                        keep.in.memory=opt.biomod.keep.in.memory,
                                        output.format=opt.biomod.output.format,
                                        on_0_1000=FALSE)

        # remove the environmental dataset to release disk space
        bccvl.remove.rasterObject(predictors)

        # save projection to output folder
        # move proj_folder
        projinput <- file.path(getwd(),
                               biomod.species.name,
                               paste("proj", projection.name, sep="_"))
        projoutput <- file.path(bccvl.env$outputdir,
                                biomod.species.name,
                                paste("proj", projection.name, sep="_"))
        # create top level dir
        dir.create(file.path(bccvl.env$outputdir, biomod.species.name))
        # move proj_future folder to output folder
        file.rename(projinput, projoutput)
        # convert grd files to tif
        bccvl.grdtogtiff(projoutput)
    }
}


# use folder name of first dataset to generate name for projection output
projectdataset(model.obj, future.climate.dataset, future.climate.data.type, future.climate.data.layer, projection.name, sdm.species)
