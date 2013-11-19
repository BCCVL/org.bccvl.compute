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


###read in the necessary observation, background and environmental data
occur = bccvl.species.read(occur.data) #read in the observation data lon/lat
bkgd = bccvl.species.read(bkgd.data) #read in the background position data lon.lat
# keep only lon and lat columns
occur = occur[c("lon","lat")]
bkgd = bkgd[c("lon","lat")]

# prepare current climate data
current.climate.scenario = stack(enviro.data.current)

# extract enviro data for species observation points and append to species data
occur = cbind(occur, extract(current.climate.scenario, cbind(occur$lon, occur$lat)))
if (!is.null(bkgd)) {
    bkgd = cbind(bkgd, extract(current.climate.scenario, cbind(bkgd$lon, bkgd$lat)))
}


###run the models and store models
############### BIOMOD2 Models ###############
# 1. Format the data
# 2. Define the model options
# 3. Compute the model
# NOTE: Model evaluation is included as part of model creation

# BIOMOD_FormatingData(resp.var, expl.var, resp.xy = NULL, resp.name = NULL, eval.resp.var = NULL,
#	eval.expl.var = NULL, eval.resp.xy = NULL, PA.nb.rep = 0, PA.nb.absences = 1000, PA.strategy = 'random',
#	PA.dist.min = 0, PA.dist.max = NULL, PA.sre.quant = 0.025, PA.table = NULL, na.rm = TRUE)
#
# resp.var a vector, SpatialPointsDataFrame (or SpatialPoints if you work with `only presences' data) containing species data (a single species) in binary format (ones for presences, zeros for true absences and NA for indeterminated ) that will be used to build the species distribution models.
# expl.var a matrix, data.frame, SpatialPointsDataFrame or RasterStack containing your explanatory variables that will be used to build your models.
# resp.xy optional 2 columns matrix containing the X and Y coordinates of resp.var (only consider if resp.var is a vector) that will be used to build your models.
# eval.resp.var	a vector, SpatialPointsDataFrame your species data (a single species) in binary format (ones for presences, zeros for true absences and NA for indeterminated ) that will be used to evaluate the models with independant data (or past data for instance).
# eval.expl.var	a matrix, data.frame, SpatialPointsDataFrame or RasterStack containing your explanatory variables that will be used to evaluate the models with independant data (or past data for instance).
# eval.resp.xy opional 2 columns matrix containing the X and Y coordinates of resp.var (only consider if resp.var is a vector) that will be used to evaluate the modelswith independant data (or past data for instance).
# resp.name	response variable name (character). The species name.
# PA.nb.rep	number of required Pseudo Absences selection (if needed). 0 by Default.
# PA.nb.absences number of pseudo-absence selected for each repetition (when PA.nb.rep > 0) of the selection (true absences included)
# PA.strategy strategy for selecting the Pseudo Absences (must be `random', `sre', `disk' or `user.defined')
# PA.dist.min minimal distance to presences for `disk' Pseudo Absences selection (in meters if the explanatory is a not projected raster (+proj=longlat) and in map units (typically also meters) when it is projected or when explanatory variables are stored within table )
# PA.dist.max maximal distance to presences for `disk' Pseudo Absences selection(in meters if the explanatory is a not projected raster (+proj=longlat) and in map units (typically also meters) when it is projected or when explanatory variables are stored within table )
# PA.sre.quant quantile used for `sre' Pseudo Absences selection
# PA.table a matrix (or a data.frame) having as many rows than resp.var values. Each column correspund to a Pseudo-absences selection. It contains TRUE or FALSE indicating which values of resp.var will be considered to build models. It must be used with `user.defined' PA.strategy.
# na.rm	logical, if TRUE, all points having one or several missing value for environmental data will be removed from analysis

# format the data as required by the biomod package
formatBiomodData = function() {
    biomod.data = rbind(occur[,c("lon", "lat")], bkgd[,c("lon", "lat")])
    biomod.data.pa = c(rep(1, nrow(occur)), rep(0, nrow(bkgd)))
    myBiomodData <-
        BIOMOD_FormatingData(resp.var =  biomod.data.pa,
                             expl.var  = stack(current.climate.scenario),
                             resp.xy   = biomod.data,
                             resp.name = 'test')  # TODO: resp.name -> species name
    return(myBiomodData)
}

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
# GLM - generalized linear model (glm)
#
###############

# myBiomodOptions <- BIOMOD_ModelingOptions(GLM = list(type = 'quadratic', interaction.level = 0, myFormula = NULL,
#	test = 'BIC', family = 'binomial', control = glm.control(epsilon = 1e-08, maxit = 1000, trace = FALSE)))
# myFormula	: a typical formula object (see example). If not NULL, type and interaction.level args are switched off
#	You can choose to either:
#	1) generate automatically the GLM formula by using the type and interaction.level arguments
#		type : formula given to the model ('simple', 'quadratic' or 'polynomial')
#		interaction.level : integer corresponding to the interaction level between variables considered. Consider that
#			interactions quickly enlarge the number of effective variables used into the GLM
#	2) or construct specific formula
# test : Information criteria for the stepwise selection procedure: AIC for Akaike Information Criteria, and BIC for Bayesian Information Criteria ('AIC' or 'BIC'). 'none' is also a supported value which implies to concider only the full model (no stepwise selection). This can lead to convergence issu and strange results.
# family : a description of the error distribution and link function to be used in the model. This can be a character string naming a family function, a family function or the result of a call to a family function. (See family for details of family functions.) BIOMOD only runs on presence-absence data so far, so binomial family by default.
# control : a list of parameters for controlling the fitting process. For glm.fit this is passed to glm.control
#	glm.control(epsilon = 1e-8, maxit = 25, trace = FALSE)
#		epsilon	- positive convergence tolerance e; the iterations converge when |dev - dev_{old}|/(|dev| + 0.1) < e
#		maxit - integer giving the maximal number of IWLS iterations
#		trace - logical indicating if output should be produced for each iteration

# 1. Format the data
myBiomodData = formatBiomodData()
# 2. Define the model options
myBiomodOptions <- BIOMOD_ModelingOptions(GLM = glm.BiomodOptions)
# 3. Compute the model
myBiomodModelOut.glm <-
    BIOMOD_Modeling(data = myBiomodData,
                    models=c('GLM'),
                    models.options=myBiomodOptions,
                    NbRunEval=biomod.NbRunEval,
                    DataSplit=biomod.DataSplit,
                    Yweights=biomod.Yweights,
                    Prevalence=biomod.Prevalence,
                    VarImport=biomod.VarImport,
                    models.eval.meth=biomod.models.eval.meth,
                    SaveObj=TRUE,
                    rescal.all.models = biomod.rescal.all.models,
                    do.full.models = biomod.do.full.models,
                    modeling.id = biomod.modeling.id)
	# model output saved as part of BIOMOD_Modeling() # EMG not sure how to retrieve
if (!is.null(myBiomodModelOut.glm)) {
    #save out the model object
    save(myBiomodModelOut.glm, file=file.path(outputdir,"model.object.RData"))
    # predict for current climate scenario
    glm.proj.c <-
        BIOMOD_Projection(modeling.output=myBiomodModelOut.glm,
                          new.env=current.climate.scenario,
                          proj.name           = 'test',  #basename(enviro.data.current), {{ species }}
                          xy.new.env = biomod.xy.new.env,
                          selected.models     = 'all',  # biomod.selected.models, {{ species }}
                          binary.meth = biomod.binary.meth,
                          filtered.meth = biomod.filtered.meth,
                          compress = biomod.compress,
                          build.clamping.mask = biomod.build.clamping.mask,
                          silent = opt.biomod.silent,
                          do.stack = opt.biomod.do.stack,
                          keep.in.memory = opt.biomod.keep.in.memory,
                          output.format = opt.biomod.output.format)
    # output is saved as part of the projection, format specified in arg 'opt.biomod.output.format'
    glm.loaded.model = BIOMOD_LoadModels(myBiomodModelOut.glm, models="GLM")
    saveBIOMODModelEvaluation(glm.loaded.model, myBiomodModelOut.glm) 	# save output
}
