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
biomod.algorithm = bccvl.params$biomod_algorithm


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
biomod.selected.models = bccvl.params$selected_models #'all' when all models have to be used to render projections or a subset vector of modeling.output models computed (eg, = grep('RF', getModelsBuiltModels(myBiomodModelOut)))
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

occlude <- function(rast, occur)
{
    vals=extract(rast, occur)
    return(occur[!is.na(vals),])
}
pixel_to_lon_lat <- function(mask, r, c)
{
    ext=extent(rasterFromCells(mask, cellFromRowCol(mask, rownr=r,colnr=c), values=T))
    return(c((ext@xmin+ext@xmax)/2.0, (ext@ymin+ext@ymax)/2.0))
}
random_valid_from_mask <- function(mask, n=1, target_val = 1)
{
    fun <- function(x) { x == target_val }
    points = rasterToPoints(mask, fun)
    indices = sample(dim(points)[1], n)
    return(data.frame(lon=points[indices,1], lat=points[indices,2]))
}

random_coords_from_raster <- function(raster, n=1)
{
    fun <- function(x) { TRUE } #todo unhack
    points = rasterToPoints(raster, fun)
    indices = sample(dim(points)[1], n)
    return(data.frame(lon=points[indices,1], lat=points[indices,2]))
}

#train_mask=raster(bccvl.params$train_mask);
#test_mask=raster(bccvl.params$test_mask);

n_occur=length(occur[[1]])
sampling=sample(n_occur, n_occur)

data_split_prop=bccvl.params$data_split/100.0
n_occur=length(occur[[1]])
sampling=sample(n_occur,n_occur)
cutoff = max(min(round(n_occur*data_split_prop),n_occur),1)
train_occur = data.frame(lon=occur$lon[sampling[1:cutoff]], lat=occur$lat[sampling[1:cutoff]])
if (cutoff < n_occur)
{
    test_occur = data.frame(lon=occur$lon[sampling[cutoff+1:n_occur]], lat=occur$lat[sampling[cutoff+1:n_occur]])
} else {
    test_occur = data.frame(lon=c(), lat=c())
}

#train.stack = crop(current.climate.scenario, extent(train_mask))
train.stack = current.climate.scenario
#test.stack = crop(current.climate.scenario, extent(test_mask))
test.stack = current.climate.scenario
# shall we use pseudo absences?
# TODO: this will ignore given absence file in case we want pseudo absences
if (bccvl.params$species_pseudo_absence_points) {
     
    train_absen = random_coords_from_raster(current.climate.scenario[[1]], 
                                            data_split_prop*bccvl.params$species_number_pseudo_absence_points)
    test_absen = random_coords_from_raster(current.climate.scenario[[1]], 
                                           (1-data_split_prop)* bccvl.params$species_number_pseudo_absence_points)
    #biomod.PA.nb.rep = 1
    #biomod.PA.nb.absences = bccvl.params$species_number_pseudo_absence_points
    biomod.PA.nb.rep = 0
    biomod.PA.nb.absences = 0
    # create an empty data frame for bkgd points
    absen = data.frame(lon=numeric(0), lat=numeric(0))
} else {
    # read absence points from file
    absen = bccvl.species.read(absen.data) #read in the background position data lon.lat
    # keep only lon and lat columns
    absen = absen[c("lon","lat")]
    n_absent=length(absen[[1]])
    sampling=sample(n_absent,n_absent)
    cutoff = max(min(round(n_absent*data_split_prop),n_absent),1)
    train_absen = data.frame(lon=absen$lon[sampling[1:cutoff]], lat=absen$lat[sampling[1:cutoff]])
    if (cutoff < n_absent)
    {
        test_absen = data.frame(lon=absen$lon[sampling[cutoff+1:n_absent]], lat=absen$lat[sampling[cutoff+1:n_absent]])
    } else {
        test_absen = data.frame(lon=c(), lat=c())
    }

}

formatBiomodData2 = function() {
    biomod.data = rbind(train_occur[,c("lon", "lat")], train_absen[,c("lon", "lat")])
    biomod.data.pa = c(rep(1, nrow(train_occur)), rep(0, nrow(train_absen)))
    biomod.test.data = rbind(test_occur[,c("lon", "lat")], test_absen[,c("lon", "lat")])
    biomod.test.data.pa = c(rep(1, nrow(test_occur)), rep(0, nrow(test_absen)))
    data_stack = stack(current.climate.scenario)
    myBiomodData <-
        BIOMOD_FormatingData(resp.var =  biomod.data.pa,
                             expl.var  = stack(train.stack),
                             resp.xy   = biomod.data,
                             resp.name = biomod.species.name,
                             PA.nb.rep = 0,
                             PA.nb.absences = 0,
                             PA.strategy = 'random',
                             eval.resp.var = biomod.test.data.pa,
                             eval.expl.var = stack(test.stack),
                             eval.resp.xy  = biomod.test.data )
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



# 1. Format the data
model.data = formatBiomodData2()
# 2. Define the model options
model.options <- bccvl.params.to.biomod.options(biomod.algorithm, bccvl.params)
# 3. Compute the model
model.sdm <-
    BIOMOD_Modeling(data = model.data,
                    models=c(biomod.algorithm),
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

#save out the model object
bccvl.save(model.sdm, name="model.object.RData")
# predict for current climate scenario

for (model in model.sdm@models.computed)
{
    model.proj = BIOMOD_Projection(modeling.output=model.sdm,
                                   new.env=current.climate.scenario,
                                   proj.name  = projection.name,  #basename(enviro.data.current), {{ species }}
                          xy.new.env = biomod.xy.new.env,
                          selected.models = model, 
                          binary.meth = biomod.binary.meth,
                          filtered.meth = biomod.filtered.meth,
                          #compress = biomod.compress,
                          build.clamping.mask = biomod.build.clamping.mask,
                          silent = opt.biomod.silent,
                          do.stack = opt.biomod.do.stack,
                          keep.in.memory = opt.biomod.keep.in.memory,
                          output.format = opt.biomod.output.format)

    # convert projection output from grd to gtiff
    index=1
    for(raster in model.proj@proj@val@layers)
    {
		# technically there should only be 1 model
        outname=file.path(getwd(),biomod.species.name,"proj_current",paste("proj.current", model, index, "tif", sep="."))
        index=index+1 
        writeRaster(raster, outname, format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)
    }
}
    
calib.dir=paste(bccvl.env$outputdir, occur.species, ".BIOMOD_DATA", biomod.modeling.id, sep="/")
# output is saved as part of the projection, format specified in arg 'opt.biomod.output.format'
loaded.model = BIOMOD_LoadModels(model.sdm, models=biomod.algorithm)
bccvl.saveBIOMODModelEvaluation(loaded.model, model.sdm, calib.dir=calib.dir) # save output

