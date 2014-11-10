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

# function to save evaluate output for BIOMOD2 models
bccvl.saveBIOMODModelEvaluation <- function(loaded.names, biomod.model, use.eval.data=FALSE) {
    # get and save the model evaluation statistics
    # EMG these must specified during model creation with the arg "models.eval.meth"
    evaluation = get_evaluations(biomod.model)
    bccvl.write.csv(evaluation, name="biomod2.modelEvaluation.csv")

    # get the model predictions and observed values
    #predictions = getModelsPrediction(biomod.model)
    predictions = get_predictions(biomod.model, evaluation=use.eval.data)
    total_models = length(dimnames(predictions)[[3]])

    # TODO: get_predictions is buggy; evaluation=FALSE works the wrong way round
    # predictions = get_predictions(biomod.model, evaluation=FALSE)
    obs = get_formal_data(biomod.model, if (use.eval.data) "eval.resp.var" else "resp.var")
    #obs = get_formal_data(biomod.model, "resp.var")
    # in case of pseudo absences we might have NA values in obs so replace them with 0
    obs = replace(obs, is.na(obs), 0)
	...
} 
roc1=bccvl.saveBIOMODModelEvaluation(loaded.model, model.sdm, use.eval.data=T) 	# save output
roc1=bccvl.saveBIOMODModelEvaluation(loaded.model, model.sdm, use.eval.data=F) 	# save output
