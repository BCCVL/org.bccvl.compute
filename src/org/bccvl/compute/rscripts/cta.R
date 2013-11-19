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
# CTA - classification tree analysis (rpart)
#
###############

# myBiomodOptions <- BIOMOD_ModelingOptions(CTA = list(method = 'class', parms = 'default', cost = NULL,
#		control = rpart.control(xval = 5, minbucket = 5, minsplit = 5, cp = 0.001, maxdepth = 25))
# method : one of "anova", "poisson", "class" or "exp". If method is missing then the routine tries to make an intelligent guess.
#	If y is a survival object, then method = "exp" is assumed, if y has 2 columns then method = "poisson" is assumed, if y is a factor then method = "class" is assumed, otherwise method = "anova" is assumed.
#	It is wisest to specify the method directly, especially as more criteria may added to the function in future.
# parms : optional parameters for the splitting function.
#	Anova splitting has no parameters.
# 	Poisson splitting has a single parameter, the coefficient of variation of the prior distribution on the rates. The default value is 1.
#	Exponential splitting has the same parameter as Poisson.
#	For classification splitting, the list can contain any of: the vector of prior probabilities (component prior), the loss matrix (component loss) or the splitting index (component split).
#		The priors must be positive and sum to 1. The loss matrix must have zeros on the diagonal and positive off-diagonal elements. The splitting index can be gini or information.
#		The default priors are proportional to the data counts, the losses default to 1, and the split defaults to gini.
# cost : a vector of non-negative costs, one for each variable in the model. Defaults to one for all variables.
#	These are scalings to be applied when considering splits, so the improvement on splitting on a variable is divided by its cost in deciding which split to choose.
# control : a list of options that control details of the rpart algorithm. See rpart.control.
#	rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, maxcompete = 4, maxsurrogate = 5,
#		usesurrogate = 2, xval = 10, surrogatestyle = 0, maxdepth = 30, ...)
# NOTE: for method and parms, you can give a 'real' value as described in the rpart help file or 'default' that implies default rpart values.

# 1. Format the data
myBiomodData = formatBiomodData()
# 2. Define the model options
myBiomodOptions <- BIOMOD_ModelingOptions(CTA = cta.BiomodOptions)
# 3. Compute the model
myBiomodModelOut.cta <-
    BIOMOD_Modeling(data = myBiomodData,
                    models = c('CTA'),
                    models.options = myBiomodOptions,
                    NbRunEval=biomod.NbRunEval,
                    DataSplit=biomod.DataSplit,
                    Yweights=biomod.Yweights,
                    Prevalence=biomod.Prevalence,
                    VarImport=biomod.VarImport,
                    models.eval.meth = biomod.models.eval.meth,
                    SaveObj = TRUE,
                    rescal.all.models = biomod.rescal.all.models,
                    do.full.models = biomod.do.full.models,
                    modeling.id = biomod.modeling.id)
if (!is.null(myBiomodModelOut.cta)) {
    #save out the model object
    save(myBiomodModelOut.cta, file=filepath(outdir,"model.object.RData"))
    # predict for current climate scenario
    cta.proj.c <-
        BIOMOD_Projection(modeling.output=myBiomodModelOut.cta,
                          new.env=current.climate.scenario,
                          proj.name= 'test', #basename(enviro.data.current), {{ species }}
                          xy.new.env = biomod.xy.new.env,
                          selected.models = 'all', # biomod.selected.models, {{ species }}
                          binary.meth = biomod.binary.meth,
                          filtered.meth = biomod.filtered.meth,
                          compress = biomod.compress,
                          build.clamping.mask = biomod.build.clamping.mask,
                          silent = opt.biomod.silent,
                          do.stack = opt.biomod.do.stack,
                          keep.in.memory = opt.biomod.keep.in.memory,
                          output.format = opt.biomod.output.format)
    # output is saved as part of the projection, format specified in arg 'opt.biomod.output.format'
    cta.loaded.model = BIOMOD_LoadModels(myBiomodModelOut.cta, models="CTA") # load model
    saveBIOMODModelEvaluation(cta.loaded.model, myBiomodModelOut.cta) 	# save output
} else {
    write(paste("FAIL!", species, "Cannot create cta model object", sep=": "), stdout())
}
