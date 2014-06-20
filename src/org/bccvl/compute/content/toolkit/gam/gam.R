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
#define the the lon/lat of the background / psuedo absence points to use -- 2 column matrix of longitude and latitude
absen.data = bccvl.params$species_absence_dataset$filename
#define the current enviro data to use
enviro.data.current = lapply(bccvl.params$environmental_datasets, function(x) x$filename)
#type in terms of continuous or categorical
enviro.data.type = lapply(bccvl.params$environmental_datasets, function(x) x$type)


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

# model-specific arguments to create a biomod model
model.options.gam <- list(
    algo = "GAM_mgcv", #"GAM_mgcv", "GAM_gam" or "BAM_mgcv"
    myFormula = NULL, #specific formula; if not NULL, type and interaction.level are args are switched off
    type = "s_smoother", #the smoother used to generate the formula; only "s_smoother" available at time; switched off if myFormula is not NULL
    interaction.level = bccvl.params$interaction_level, #integer corresponding to the interaction level between variables considered; switched off if myFormula is not NULL
    k = NULL, #a smooth term in a formula argument to gam (see gam s or mgcv s)
    family = bccvl.params$family, #"binomial", "bernoulli", "gaussian", "laplace", "tdist", "huberized", "multinomial", "adaboost", "poisson", "coxph", "quantile", or "pairwise"
    control = list(
        # Specific GAM settings
        irls.reg = bccvl.params$irls_reg, #the size of the ridge regression penalty to the model to impose identifiability; for most models this should be 0
        epsilon = bccvl.params$epsilon, #this is used for judging conversion of the GLM IRLS loop
        maxit = bccvl.params$maxit, #maximum number of IRLS iterations to perform
        mgcv.tol = bccvl.params$mgcv_tol, #the convergence tolerance parameter to use in GCV/UBRE optimization
        mgcv.half = bccvl.params$mgcv_half, #if a step of the GCV/UBRE optimization method leads to a worse GCV/UBRE score, then the step length is halved; this is the number of halvings to try before giving up
        trace = FALSE, #set this to TRUE to turn on diagnostic output
        rank.tol = .Machine$double.eps^0.5, #the tolerance used to estimate the rank of the fitting problem
        nlm = list(), #list of control parameters to pass to nlm if this is used for outer estimation of smoothing parameters (not default)
        optim = list(), #list of control parameters to pass to optim if this is used for outer estimation of smoothing parameters (not default)
        newton = list(), #list of control parameters to pass to default Newton optimizer used for outer estimation of log smoothing parameters
        outerPIsteps = 0, #the number of performance interation steps used to initialize outer iteration
        idLinksBases = TRUE, #if smooth terms have their smoothing parameters linked via the id mechanism (see s), should they also have the same bases. Set this to FALSE only if you are sure you know what you are doing
        scalePenalty = TRUE, #this option rescales the penalty matrices to accomodate this problem. Probably should be set to FALSE if you are linking smoothing parameters but have set idLinkBases to FALSE
        keepData = FALSE #should a copy of the original data argument be kept in the gam object
    )
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
biomod.build.clamping.mask = TRUE #if TRUE, a clamping mask will be saved on hard drive
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
current.climate.scenario = bccvl.enviro.stack(enviro.data.current)

###read in the necessary observation, background and environmental data
occur = bccvl.species.read(occur.data) #read in the observation data lon/lat
# keep only lon and lat columns
occur = occur[c("lon","lat")]

# shall we use pseudo absences?
# TODO: this will ignore given absence file in case we want pseudo absences
if (bccvl.params$species_pseudo_absence_points) {
    biomod.PA.nb.rep = 1
    biomod.PA.nb.absences = bccvl.params$species_number_pseudo_absence_points
    # create an empty data frame for bkgd points
    absen = data.frame(lon=numeric(0), lat=numeric(0))
} else {
    # read absence points from file
    absen = bccvl.species.read(absen.data) #read in the background position data lon.lat
    # keep only lon and lat columns
    absen = absen[c("lon","lat")]
}

# extract enviro data for species observation points and append to species data
occur = cbind(occur, extract(current.climate.scenario, cbind(occur$lon, occur$lat)))
if (!is.null(absen)) {
    absen = cbind(absen, extract(current.climate.scenario, cbind(absen$lon, absen$lat)))
}


###run the models and store models
############### BIOMOD2 Models ###############
# 1. Format the data
# 2. Define the model options
# 3. Compute the model
# NOTE: Model evaluation is included as part of model creation

# BIOMOD_FormatingData(resp.var, expl.var, resp.xy = NULL, resp.name = NULL, eval.resp.var = NULL,
#   eval.expl.var = NULL, eval.resp.xy = NULL, PA.nb.rep = 0, PA.nb.absences = 1000, PA.strategy = 'random',
#   PA.dist.min = 0, PA.dist.max = NULL, PA.sre.quant = 0.025, PA.table = NULL, na.rm = TRUE)
#
# resp.var a vector, SpatialPointsDataFrame (or SpatialPoints if you work with `only presences' data) containing species data (a single species) in binary format (ones for presences, zeros for true absences and NA for indeterminated ) that will be used to build the species distribution models.
# expl.var a matrix, data.frame, SpatialPointsDataFrame or RasterStack containing your explanatory variables that will be used to build your models.
# resp.xy optional 2 columns matrix containing the X and Y coordinates of resp.var (only consider if resp.var is a vector) that will be used to build your models.
# eval.resp.var a vector, SpatialPointsDataFrame your species data (a single species) in binary format (ones for presences, zeros for true absences and NA for indeterminated ) that will be used to evaluate the models with independant data (or past data for instance).
# eval.expl.var a matrix, data.frame, SpatialPointsDataFrame or RasterStack containing your explanatory variables that will be used to evaluate the models with independant data (or past data for instance).
# eval.resp.xy opional 2 columns matrix containing the X and Y coordinates of resp.var (only consider if resp.var is a vector) that will be used to evaluate the modelswith independant data (or past data for instance).
# resp.name response variable name (character). The species name.
# PA.nb.rep number of required Pseudo Absences selection (if needed). 0 by Default.
# PA.nb.absences number of pseudo-absence selected for each repetition (when PA.nb.rep > 0) of the selection (true absences included)
# PA.strategy strategy for selecting the Pseudo Absences (must be `random', `sre', `disk' or `user.defined')
# PA.dist.min minimal distance to presences for `disk' Pseudo Absences selection (in meters if the explanatory is a not projected raster (+proj=longlat) and in map units (typically also meters) when it is projected or when explanatory variables are stored within table )
# PA.dist.max maximal distance to presences for `disk' Pseudo Absences selection(in meters if the explanatory is a not projected raster (+proj=longlat) and in map units (typically also meters) when it is projected or when explanatory variables are stored within table )
# PA.sre.quant quantile used for `sre' Pseudo Absences selection
# PA.table a matrix (or a data.frame) having as many rows than resp.var values. Each column correspund to a Pseudo-absences selection. It contains TRUE or FALSE indicating which values of resp.var will be considered to build models. It must be used with `user.defined' PA.strategy.
# na.rm logical, if TRUE, all points having one or several missing value for environmental data will be removed from analysis

# format the data as required by the biomod package
formatBiomodData = function() {
    biomod.data = rbind(occur[,c("lon", "lat")], absen[,c("lon", "lat")])
    biomod.data.pa = c(rep(1, nrow(occur)), rep(0, nrow(absen)))
    myBiomodData <-
        BIOMOD_FormatingData(resp.var =  biomod.data.pa,
                             expl.var  = stack(current.climate.scenario),
                             resp.xy   = biomod.data,
                             resp.name = biomod.species.name,
                             PA.nb.rep = biomod.PA.nb.rep,
                             PA.nb.absences = biomod.PA.nb.absences,
                             PA.strategy = 'random')
    return(myBiomodData)
}

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
# GAM - generalized additive model (gam::gam, mgcv::gam or mgcv::bam)
#
###############

# myBiomodOptions <- BIOMOD_ModelingOptions(GAM = list(algo = 'GAM_mgcv', type = 's_smoother', k = NULL, 
#   interaction.level = 0, myFormula = NULL, family = 'binomial', 
#   control = gam.control(epsilon = 1e-06, trace = FALSE, maxit = 100)))
# algo : either "GAM_mgcv" (default), "GAM_gam" or "BAM_mgcv" defining the chosen GAM function (see gam, gam resp. bam for more details)
# myFormula : a typical formula object (see example). If not NULL, type and interaction.level args are switched off. 
#   You can choose to either:
#   1) generate automatically the GAM formula by using the type and interaction.level arguments 
#       type : the smother used to generate the formula. Only "s_smoother" available at time. 
#       interaction.level : integer corresponding to the interaction level between variables considered. Consider that interactions quickly enlarge the number of effective variables used into the GAM. Interaction are not considered if you choosed "GAM_gam" algo
#   2) or construct specific formula
# k : a smooth term in a formula argument to gam (see gam s or mgcv s)
# family : a description of the error distribution and link function to be used in the model. This can be a character string naming a family function, a family function or the result of a call to a family function. (See family for details of family functions.) BIOMOD only runs on presence-absence data so far, so binomial family by default.
# control : see gam::gam.control or mgcv::gam.control
#   gam::gam.control(epsilon=1e-07, bf.epsilon = 1e-07, maxit=30, bf.maxit = 30, trace=FALSE,...)
#   mgcv::gam.control(irls.reg=0.0,epsilon = 1e-06, maxit = 100, mgcv.tol=1e-7,mgcv.half=15, trace = FALSE, rank.tol=.Machine$double.eps^0.5, nlm=list(), optim=list(), newton=list(), outerPIsteps=0, idLinksBases=TRUE, scalePenalty=TRUE, keepData=FALSE) 

# 1. Format the data
model.data = formatBiomodData()
# 2. Define the model options
model.options <- BIOMOD_ModelingOptions(GAM = model.options.gam)
# 3. Compute the model
model.sdm <-
    BIOMOD_Modeling(data = model.data,
                    models=c('GAM'),
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
                      output.format = opt.biomod.output.format)
# convert projection output from grd to gtiff
bccvl.grdtogtiff(file.path(getwd(),
                           biomod.species.name,
                           paste("proj", projection.name, sep="_")))


# output is saved as part of the projection, format specified in arg 'opt.biomod.output.format'
loaded.model = BIOMOD_LoadModels(model.sdm, models="GAM")
bccvl.saveBIOMODModelEvaluation(loaded.model, model.sdm)    # save output
