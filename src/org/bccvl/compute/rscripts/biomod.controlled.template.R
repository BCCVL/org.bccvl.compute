# FIXME: R env setup should be done on compute host
#        - lib dir: get rid of it
#        - don't do install.packages?
#        -
# setup R environment
#if (!file.exists(Sys.getenv("R_LIBS_USER"))) {
#    dir.create(Sys.getenv("R_LIBS_USER"), recursive=TRUE);
#}
#.libPaths(Sys.getenv("R_LIBS_USER"))
# set CRAN mirror in case we need to download something

## TODO: setup CRAN mirror in .Renviron
## see http://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html
## don't install here... just require

r <- getOption("repos")
r["CRAN"] <- "http://cran.ms.unimelb.edu.au/"
options(repos=r)


#script to run to develop distribution models
###check if libraries are installed, install if necessary and then load them
necessary=c("ggplot2","tools", "rjson", "dismo","SDMTools", "gbm", "rgdal", "pROC", "R2HTML", "png", "biomod2") #list the libraries needed
installed = necessary %in% installed.packages() #check if library is installed
if (length(necessary[!installed]) >=1) {
    install.packages(necessary[!installed], dep = T) #if library is not installed, install it
}
for (lib in necessary) {
    library(lib,character.only=T) #load the libraries
}

# turn BCCVL parameters into BIOMOD options
bccvl.params.to.biomod.options <- function(method,bccvl.params)
{
    biomod.options = NULL
    
    if (method == "RF") {

        biomod.options.list = list(
                            do.classif =  bccvl.params$do.classif,
                            ntree =  bccvl.params$ntree,
                            mtry = if (bccvl.params$mtry == "default") bccvl.params$mtry else as.integer(bccvl.params$mtry),
                            nodesize=  bccvl.params$nodesize,
                            maxnodes = bccvl.params$maxnodes)
        biomod.options = BIOMOD_ModelingOptions(RF = biomod.options.list)

    } else if (method == "MARS") {

        biomod.options.list = list(
                            degree =  bccvl.params$degree,
                            nk =  bccvl.params$nk,
                            penalty= bccvl.params$penalty,
                            thresh = bccvl.params$thresh,
                            prune = bccvl.params$prune)
        biomod.options = BIOMOD_ModelingOptions(MARS = biomod.options.list)

    } else if (method == "SRE") {

        biomod.options.list = list(Quant = bccvl.params$Quant)
        biomod.options = BIOMOD_ModelingOptions(SRE = biomod.options.list)

    } else if (method == "MAXENT") {

        biomod.options.list = list(
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
                            defaultprevalence = bccvl.params$defaultprevalence)
        biomod.options = BIOMOD_ModelingOptions(MAXENT = biomod.options.list)

    } else if (method == "GLM") {

        biomod.options.list = list(
                            type = bccvl.params$type,    #"simple", "quadratic" or "polynomial"; switched off if myFormula is not NULL
                            interaction.level = bccvl.params$interaction_level, #integer corresponding to the interaction level between variables considered; switched off if myFormula is not NULL
                            # myFormula = NULL, #specific formula; if not NULL, type and interaction.level are args are switched off
                                  test = bccvl.params$test, #"AIC", "BIC" or "none"
                            family = bccvl.params$family, #"binomial", "gaussian", "gamma", "inverse.gaussian", "poisson", "quasi", "quasibinomial", "quasipoisson"
                            mustart = bccvl.params$mustart, #starting values for the vector of means
                            control = list(
                                    epsilon = bccvl.params$control_epsilon, #positive convergence tolerance e
                                    maxit = bccvl.params$control_maxit, #integer giving the maximal number of IWLS iterations
                                    trace = bccvl.params$control_trace #logical indicating if output should be produced for each iteration
                                    ))
        biomod.options = BIOMOD_ModelingOptions(GLM = biomod.options.list)

    } else if (method == "GBM") {

        biomod.options.list = list(
                            distribution = bccvl.params$distribution, # "bernoulli", "gaussian", "laplace", "tdist", "huberized", "multinomial", "adaboost", "poisson", "coxph", "quantile", or "pairwise"
                            n.trees = bccvl.params$n_trees, # The total number of trees to fit
                            interaction.depth = bccvl.params$interaction_depth, # Maximum depth of variable interactions
                            n.minobsinnode = bccvl.params$n_minobsinnode,
                            shrinkage = bccvl.params$shrinkage, # A shrinkage parameter applied to each tree in the expansion
                            bag.fraction = bccvl.params$bag_fraction, # The fraction of the training set observations randomly selected to propose the next tree in the expansion
                            train.fraction = bccvl.params$train_fraction, # The first train.fraction * nrows(data) observations are used to fit the gbm
                            cv.folds = bccvl.params$cv_folds # Number of cross-validation folds to perform
                            )
        biomod.options = BIOMOD_ModelingOptions(GBM = biomod.options.list)

    } else if (method == "GAM") {

        biomod.options.list = list(algo = "GAM_mgcv", #"GAM_mgcv", "GAM_gam" or "BAM_mgcv"
                            myFormula = NULL, #specific formula; if not NULL, type and interaction.level are args are switched off
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
                                ),
                            type = "s_smoother", #the smoother used to generate the formula; only "s_smoother" available at time; switched off if myFormula is not NULL
                            interaction.level = bccvl.params$interaction_level #integer corresponding to the interaction level between variables considered; switched off if myFormula is not NULL
                            )
        biomod.options = BIOMOD_ModelingOptions(GAM = biomod.options.list)

    } else if (method == "FDA") {

        biomod.options.list = list(method = bccvl.params$method #regression method used in optimal scaling; "polyreg", "mars", "bruto" or "gen.ridge"
                                  )
        biomod.options = BIOMOD_ModelingOptions(FDA = biomod.options.list)

    } else if (method == "CTA") {

        biomod.options.list = list(
                            method = bccvl.params$method, #"anova", "poisson", "class" or "exp"
                            # parms = "default", #optional parameters for the splitting function
                            # cost = NULL, #a vector of non-negative costs, one for each variable in the model. Defaults to one for all variables
                            control = list(
                                        xval = bccvl.params$control_xval, #number of cross-validations
                                        minbucket = bccvl.params$control_minbucket, #the minimum number of observations in any terminal <leaf> node
                                        minsplit = bccvl.params$control_minsplit, #the minimum number of observations that must exist in a node in order for a split to be attempted
                                        cp = bccvl.params$control_cp, #complexity parameter
                                        maxdepth = bccvl.params$control_maxdepth  #Set the maximum depth of any node of the final tree, with the root node counted as depth 0. Values greater than 30 rpart will give nonsense results on 32-bit machines))
                                        )
                            )
        biomod.options = BIOMOD_ModelingOptions(CTA = biomod.options.list)

    } else if (method == "ANN") {

        biomod.options.list = list(
                            NbCV = bccvl.params$nbcv, #nb of cross validation to find best size and decay parameters
                            rang = bccvl.params$rang, #Initial random weights on [-rang, rang]
                            maxit = bccvl.params$maxit #maximum number of iterations. Default 100)
                            )
        biomod.options = BIOMOD_ModelingOptions(ANN = biomod.options.list)

    } else {
        sprintf("Unknown method: %s\n",method)
    }
    return (biomod.options)
}


# load parameters
params = rjson::fromJSON(file="params.json")
bccvl.params <- params$params
bccvl.env <- params$env
rm(params)
# set working directory (script runner takes care of it)
setwd(bccvl.env$outputdir)

############################################################
#
# define helper functions to use in bccvl
#
############################################################

## Needed for tryCatch'ing:
bccvl.err.null <- function (e) return(NULL)

# read species presence/absence data
#    return NULL if filename is not  given
# TODO: shall we set projection here as well? use SpatialPoints?
bccvl.species.read <- function(filename) {
    if (!is.null(filename)) {
        # We might loose precision of lon/lat when ronverting to double,
        # However, given the nature of the numbers, and the resolution of raster files
        # we deal with, this shouldn't be a problem.
        return (read.csv(filename, colClasses=c("lon"="numeric", "lat"="numeric")))
        #return (read.csv(filename))
    }
}

# use either absen.data (1) or generate random pseudo absence points (2)
# (1) extract "lat" and "lon" from absen.data
# (2) generate number of absence points in area of climate.data
bccvl.dismo.absence <- function(absen.data=NULL,
                                pseudo.absen.enabled=FALSE,
                                pseudo.absen.points=0,
                                climate.data=NULL,
                                occur.data=NULL) {
    # TODO: combine random and given absence points:
    # rbind(absen.datafromfile, bkgd.datarandom)
    if (pseudo.absen.enabled) {
        # generate randomPoints
        bkgd = randomPoints(
            climate.data,
            pseudo.absen.points,
            occur.data)
        # as data frame
        absen = as.data.frame(bkgd)
        # rename columns
        names(absen) <- c("lon","lat")
    } else {
        # otherwise read absence ponits from file
        absen = bccvl.species.read(absen.data) #read in the background position data lon.lat
        # keep only lon and lat columns
        absen = absen[c("lon","lat")]
    }
    return(absen);
}

# return a RasterStack of given vector of input files
# Potentially crops the data to an automatically determined
# common (mutual) extent.
# Checks if data has projection associated with it, and in case
# there is none, this method sets it to WGS84 (EPSG:4326)
bccvl.enviro.stack <- function(filenames) {
    
    raster.list=lapply(filenames, raster) # raster is lazy to load images
    extent.list=lapply(raster.list, extent)
    
    common.extent=extent.list[[1]]
    equal.extents=TRUE
    i=2
    while (i<=length(extent.list))
    {
        rast.extent=extent.list[[i]]
        if (rast.extent != common.extent) equal.extents = FALSE
        common.extent=intersect(common.extent, rast.extent)
        i=i+1
    }

    if (equal.extents)
    {
        # Fast path
        raster.stack=stack(raster.list)
    } else {
        warning_msg=sprintf("Raster stack: enforcing common extent xmin,xmax=[%f, %f] ymin,ymax=[%f, %f]", 
                            common.extent@xmin,
                            common.extent@xmax,
                            common.extent@ymin,
                            common.extent@ymax)
        warning(warning_msg, immediate=TRUE)
                         
        raster.stack=stack()
        for (rast in raster.list)
        {
            raster.stack = stack(raster.stack, crop(rast, common.extent))
            #raster.stack = addLayer(raster.stack, crop(rast, common.extent))  # equivalent?
        }
    }

    proj = proj4string(raster.stack)

    if (is.na(proj)) {
        # None set, let's set a default
        proj4string(raster.stack) <- CRS("+init=epsg:4326")
        warning("Environmental data set has no projection metadata. Set to default EPSG:4326")
    }
    
    return (raster.stack)
}

# function to save projection output raster
bccvl.saveModelProjection <- function(model.obj, projection.name, species, outputdir=bccvl.env$outputdir) {
    ## save projections under biomod2 compatible name:
    ##  proj_name_species.tif
    ##  only useful for dismo outputs
    basename = paste("proj", projection.name, species, sep="_")
    filename = file.path(outputdir, paste(basename, 'tif', sep="."))
    writeRaster(model.obj, filename, format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)
}

# function to save RData in outputdir
bccvl.save <- function(robj, name, outputdir=bccvl.env$outputdir) {
    filename = file.path(outputdir, name)
    save(robj, file=filename)
}

# function to save CSV Data in outputdir
bccvl.write.csv <- function(robj, name, outputdir=bccvl.env$outputdir) {
    filename = file.path(outputdir, name)
    write.csv(robj, file=filename)
}

# function to get model object
bccvl.getModelObject <- function(model.file=bccvl.env$inputmodel) {
    return (get(load(file=model.file)))
}

# convert all .gri/.grd found in folder to gtiff
# TODO: extend to handle other grid file formats, e.g. .asc
bccvl.grdtodisk <- function(folder) {
    grdfiles <- list.files(path=folder,
                           pattern="^.*\\.gri")
    for (grdfile in grdfiles) {
        # get grid file name
        print("here")
        print(grdfile)
        grdname <- file_path_sans_ext(grdfile)
        # read grid raster
        grd <- raster(file.path(folder, grdfile))
        # write raster as geotiff
        filename = file.path(folder, paste(grdname, 'tif', sep="."))
        writeRaster(grd, filename, format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

        png_filename = file.path(folder, paste(grdname, 'png', sep="."))
        plot(grd)
        title(grdname)
        dev.off()
        # remove grd files
        file.remove(file.path(folder, paste(grdname, c("grd","gri"), sep=".")))
    }
}
############################################################
#
# define helper functions for projections
#
############################################################

# function to check that the environmental layers used to project the
# model are the same as the ones used to create the model object
#    model.obj     ... model to project
#    climatelayers ... climate data to project onto
bccvl.checkModelLayers <- function(model.obj, climatelayers) {
    message("Checking environmental layers used for projection")
    # get the names of the environmental layers from the original model
    if (inherits(model.obj, "DistModel")) {
        # dismo package
        model.layers = colnames(model.obj@presence)
    } else if (inherits(model.obj, "gbm")) {
        # brt package
        model.layers = summary(model.obj)$var
    } else if (inherits(model.obj, "BIOMOD.models.out")) {
        # biomod package
        model.layers = model.obj@expl.var.names
    }

    # get the names of the climate scenario's env layers
    pred.layers = names(climatelayers)

    # check if the env layers were in the original model
    if(sum(!(pred.layers %in% model.layers)) > 0 ){
        message("Dropping environmental layers not used in the original model creation...")
        # create a new list of env predictors by dropping layers not in the original model
        new.predictors = climatelayers
        for (pl in pred.layers) {
            if (!(pl %in% model.layers)) {
                new.predictors = dropLayer(new.predictors, pl)
            }
        }
        return(new.predictors)
    } else {
        return(climatelayers)
    }
}
######################################################################################
# model accuracy helpers
######################################################################################

# ROC    Relative Operating Characteristic
# KAPPA (HSS)  Cohen's Kappa (Heidke skill score)
# TSS (HK, PSS)    True skill statistic (Hanssen and Kuipers discriminant, Peirce's skill score)
# FAR    False alarm ratio
# SR     Success ratio
# ACCURACY Accuracy (fraction correct)
# BIAS   Bias score (frequency bias)
# POD    Probability of detection (hit rate)
# CSI    Critical success index (threat score)
# ETS    Equitable threat score (Gilbert skill score)
# POFD   Probability of false detection (false alarm rate)
# OR     Odds ratio
# ORSS   Odds ratio skill score (Yule's Q)
# unsupported?
# http://www.cawcr.gov.au/projects/verification/#Methods_for_dichotomous_forecasts
# BOYCE .. not implemented?
# TS       Threat score (critical success index)

# function to save evaluate output
bccvl.saveModelEvaluation <- function(out.model, out.biomod.model) {
    # save the 'dismo::ModelEvalution' object
    bccvl.save(out.model, name="dismo.eval.object.RData")
    # save all the model accuracy statistics provided in both dismo and biomod2
    rownames(out.biomod.model) <- c("Testing.data", "Cutoff", "Sensitivity", "Specificity")
    bccvl.write.csv(t(round(out.biomod.model, digits=3)), name="combined.modelEvaluation.csv")
    # EMG no guarantee these value are correct

    # save AUROC curve
    png(file=file.path(bccvl.env$outputdir, 'AUC.png'))
    plot(out.model, 'ROC');
    dev.off()
}

#
# returns:
#
#    best.iter: the best score obtained for chosen statistic
#    cutoff: the associated cut-off used for transform fitted vector into binary
#    sensibility: the sensibility with this threshold
#    specificity: the specificity with this threshold
#
# Note this function looks to be based on biomod2's Find.Optim.Stat function - see that
# for the original reference
#
bccvl.Find.Optim.Stat <- function(Stat='TSS', Fit, Obs, Precision=5, Fixed.thresh=NULL) {
    uniform_obs=length(unique(Obs)) == 1
    uniform_fit=length(unique(Fit)) == 1
    if( uniform_obs | uniform_fit ) {
        msg=sprintf("Stat: %s.", Stat)
        if (uniform_obs) msg=sprintf("%s Uniform observed data.", msg)
        if (uniform_fit) msg=sprintf("%s Uniform fitted data.", msg)
        msg=sprintf("%s Be careful with this model's predictions.", msg)
        # warning("\nObserved or fited data contains only a value.. Evaluation Methods switched off\n",immediate.=T)
        # best.stat <- cutoff <- true.pos <- sensibility <- true.neg <- specificity <- NA
        #warning("\nObserved or fitted data contains a unique value.. Be carefull with this model's predictions\n",immediate.=T)
        warning(sprintf("\n%s\n", msg),immediate.=T)
        #best.stat <- cutoff <- true.pos <- sensibility <- true.neg <- specificity <- NA
    } #else {
    if(Stat != 'ROC'){
        StatOptimum <- bccvl.getStatOptimValue(Stat)
        if(is.null(Fixed.thresh)){ # test a range of threshold to get the one giving the best score
            if(length(unique(Fit)) == 1){
                valToTest <- unique(Fit)
                valToTest <- round(c(mean(c(0,valToTest)), mean(c(1000,valToTest))))
            } else{
                mini <- max(min(quantile(Fit,0.05, na.rm=T), na.rm=T),0)
                maxi <- min(max(quantile(Fit,0.95, na.rm=T), na.rm=T),1000)
                # valToTest <- unique( round(c(seq(mini,maxi,length.out=100), mini, maxi)) )
                # EMG no idea why the round() is here, it makes vals between 0 and 1 (ie bioclim) all 0
                valToTest <- unique( c(seq(mini,maxi,length.out=100)))
                # deal with unique value to test case
                if(length(valToTest)<3){
                    valToTest <- round(c(mean(0,mini), valToTest, mean(1000,maxi)))
                }
            }
            # valToTest <- unique( c(seq(mini,maxi,by=Precision), mini, maxi) )
        } else{
            valToTest <- Fixed.thresh
        }
        calcStat <- sapply(lapply(valToTest, function(x){return(table(Fit>x,Obs))} ), bccvl.calculate.stat, stat=Stat)

        # scal on 0-1 ladder.. 1 is the best
        calcStat <- 1 - abs(StatOptimum - calcStat)

        best.stat <- max(calcStat, na.rm=T)

        cutoff <- median(valToTest[which(calcStat==best.stat)]) # if several values are selected

        misc <- table(Fit >= cutoff, Obs)
        misc <- bccvl.contagency.table.check(misc)
        true.pos <- misc['TRUE','1']
        true.neg <- misc['FALSE','0']
        specificity <- (true.neg * 100)/sum(misc[,'0'])
        sensibility <- (true.pos * 100)/sum(misc[,'1'])
    } else{
        roc1 <- roc(Obs, Fit, percent=T)
        roc1.out <- coords(roc1, "best", ret=c("threshold", "sens", "spec"))
        best.stat <- as.numeric(auc(roc1))/100
        cutoff <- as.numeric(roc1.out["threshold"])
        sensibility <- as.numeric(roc1.out["sensitivity"])
        specificity <- as.numeric(roc1.out["specificity"])
    }
   #}
    return(cbind(best.stat,cutoff,sensibility,specificity))
}

bccvl.getStatOptimValue <- function(stat) {
    if(stat == 'TSS') return(1)
    if(stat == 'KAPPA') return(1)
    if(stat == 'ACCURACY') return(1)
    if(stat == 'BIAS') return(1)
    if(stat == 'POD') return(1)
    if(stat == 'FAR') return(0)
    if(stat == 'POFD') return(0)
    if(stat == 'SR') return(1)
    if(stat == 'CSI') return(1)
    if(stat == 'ETS') return(1)
    if(stat == 'HK') return(1)
    if(stat == 'HSS') return(1)
    if(stat == 'OR') return(1000000)
    if(stat == 'ORSS') return(1)

    #dismo
    if(stat == 'ODP') return(1)
    # if(stat == 'CCR') return(1) # same as ACCURACY
    # if(stat == 'TPR') return(1) # same as POD
    if(stat == 'TNR') return(1)
    if(stat == 'FPR') return(0)
    if(stat == 'FNR') return(0)
    # if(stat == 'PPP') return(1) # same as SR
    if(stat == 'NPP') return(1)
    if(stat == 'MCR') return(0)
    if(stat == 'OR') return(1000000)
    # if(stat == 'kappa') return(1) # same as KAPPA
}

bccvl.calculate.stat <- function(Misc, stat='TSS') {
    # Contagency table checking
    Misc <- bccvl.contagency.table.check(Misc)

    # Defining Classification index
    hits <- Misc['TRUE','1']
    misses <- Misc['FALSE','1']
    false_alarms <- Misc['TRUE','0']
    correct_negatives <- Misc['FALSE','0']

    total <- sum(Misc)
    forecast_1 <- sum(Misc['TRUE',])
    forecast_0 <- sum(Misc['FALSE',])
    observed_1 <- sum(Misc[,'1'])
    observed_0 <- sum(Misc[,'0'])

    # Calculating choosen evaluating metric
    if(stat=='TSS') {
        return( (hits/(hits+misses)) + (correct_negatives/(false_alarms+correct_negatives)) -1 )
    }

    if(stat=='KAPPA') {
        Po <- (1/total) * (hits + correct_negatives)
        Pe <- ((1/total)^2) * ((forecast_1 * observed_1) + (forecast_0 * observed_0))
        return( (Po - Pe) / (1-Pe) )
    }

    if(stat=='ACCURACY') {
        return( (hits + correct_negatives) / total)
    }

    if(stat=='BIAS') {
        return( (hits + false_alarms) / (hits + misses))
    }

    if(stat=='POD') {
        return( hits / (hits + misses))
    }

    if(stat=='FAR') {
        return(false_alarms/(hits+false_alarms))
    }

    if(stat=='POFD') {
        return(false_alarms / (correct_negatives + false_alarms))
    }

    if(stat=='SR') {
        return(hits / (hits + false_alarms))
    }

    if(stat=='CSI') {
        return(hits/(hits+misses+false_alarms))
    }

    if(stat=='ETS') {
        hits_rand <- ((hits+misses)*(hits+false_alarms)) / total
        return( (hits-hits_rand) / (hits+misses+false_alarms-hits_rand))
    }

    # if(stat=='HK') {
    # return((hits/(hits+misses)) - (false_alarms/(false_alarms + correct_negatives)))
    # }

    # if(stat=='HSS') {
    # expected_correct_rand <- (1/total) * ( ((hits+misses)*(hits+false_alarms)) +
    # ((correct_negatives + misses)*(correct_negatives+false_alarms)) )
    # return((hits+correct_negatives-expected_correct_rand) / (total - expected_correct_rand))
    # }

    # if(stat=='OR') {
    # return((hits*correct_negatives)/(misses*false_alarms))
    # }

    # if(stat=='ORSS') {
    # return((hits*correct_negatives - misses*false_alarms) / (hits*correct_negatives + misses*false_alarms))
    # }

    # if(stat=="BOYCE") {
    #
    # }

    #dismo
    if(stat=='ODP') {
        return((false_alarms + correct_negatives) / total)
    }

    # if(stat=='CCR') {
    # return((hits + correct_negatives) / total)
    # }

    # if(stat=='TPR') {
    # return(hits / (hits + misses))
    # }

    if(stat=='TNR') {
        return(correct_negatives / (false_alarms + correct_negatives))
    }

    if(stat=='FPR') {
        return(false_alarms / (false_alarms + correct_negatives))
    }

    if(stat=='FNR') {
        return(misses / (hits + misses))
    }

    # if(stat=='PPP') {
    # return(hits / (hits + false_alarms))
    # }

    if(stat=='NPP') {
        return(correct_negatives / (misses + correct_negatives))
    }

    if(stat=='MCR') {
        return((false_alarms + misses) / total)
    }

    if(stat=='OR') {
        return((hits * correct_negatives) / (misses * false_alarms))
    }

    # if(stat=='kappa') {
    # return(((hits + correct_negatives) - (((hits + misses)*(hits + false_alarms) + (false_alarms + correct_negatives)*(misses + correct_negatives)) / total)) /
    # (total -(((hits + misses)*(hits + false_alarms) + (false_alarms + correct_negatives)*(misses + correct_negatives)) / total)))
    # }
}

bccvl.contagency.table.check <- function(Misc) {
    # Contagency table checking
    if(dim(Misc)[1]==1) {
        if(row.names(Misc)[1]=="FALSE") {
            Misc <- rbind(Misc, c(0,0))
            rownames(Misc) <- c('FALSE','TRUE')
        } else {
            a <- Misc
            Misc <- c(0,0)
            Misc <- rbind(Misc, a)
            rownames(Misc) <- c('FALSE','TRUE')
        }
    }

    if(ncol(Misc) != 2 | nrow(Misc) !=2 ) {
        Misc = matrix(0, ncol=2, nrow=2, dimnames=list(c('FALSE','TRUE'), c('0','1')))
    }

    if((sum(colnames(Misc) %in% c('FALSE','TRUE','0','1')) < 2) | (sum(rownames(Misc) %in% c('FALSE','TRUE','0','1')) < 2) ){
        stop("Unavailable contagency table given")
    }

    if('0' %in% rownames(Misc)) rownames(Misc)[which(rownames(Misc)=='0')] <- 'FALSE'
    if('1' %in% rownames(Misc)) rownames(Misc)[which(rownames(Misc)=='1')] <- 'TRUE'

    return(Misc)
}

# function to generate marginal (mean) response curves for dismo models
# i.e., hold all but one predictor variable to its mean value and recalculate model predictions
bccvl.createMarginalResponseCurves <- function(out.model, model.name) {
   # get the enviromental variables and values used to create the model
    if (model.name == "brt") {
        model.values = matrix(out.model$data$x, ncol=length(out.model$var.names))
        env.vars = out.model$var.names
    } else if (model.name %in% c("geoIDW", "voronoiHull")) {
        model.values = rbind(out.model@presence, out.model@absence)
        env.vars = colnames(model.values)
    } else {
        model.values = out.model@presence
        env.vars = colnames(model.values)
    }

    if (!(length(model.values)==0)) {

        # create a matrix to hold average values for each environmental variable
        mean.values = matrix(data = NA, nrow = 100, ncol = length(env.vars))
        colnames(mean.values) = env.vars
        # for each variable, populate the column with the mean value
        for (i in 1:ncol(mean.values)) {
            mean.values[,i] = rep(mean(model.values[,i], na.rm=TRUE), 100)
        }

        # allow each environmental variable to vary, keeping other variable values at average, and predict suitability
        for (j in 1:ncol(mean.values)) {
            range.values = seq(min(model.values[,j], na.rm=TRUE), max(model.values[,j], na.rm=TRUE), length.out=100)
            temp.data = mean.values
            temp.data[,j] = range.values
            if (model.name == "brt") {
                colnames(temp.data) = env.vars
                new.predictions = predict(out.model, as.data.frame(temp.data), n.trees = out.model$gbm.call$best.trees, type = "response")
            } else {
                new.predictions = predict(out.model, temp.data)
            }

            # create separate file for each response curve
            save.name = env.vars[j]
            png(file=file.path(bccvl.env$outputdir, paste(save.name, "_response.png", sep="")))
            plot(range.values, new.predictions, ylim=c(0,1), xlab="", ylab="", main=save.name, type="l")
            rug(model.values[,j])
            dev.off()
        }
    } else {
        write(paste(species, ": Cannot create response curves from", model.name, "object", sep=" "), stdout())
    }
}

# function to calculate variable importance values for dismo models based on biomod2's correlation between predictions
# i.e., hold all but one predictor variable to its actual values, resample that one predictor and recalculate model predictions
bccvl.calculateVariableImpt <- function(out.model, model.name, num_samples) {
    # EMG num_samples should be same as biomod.VarImport arg set in
    # 01.init.args.model.current.R

    # get the enviromental variables and values used to create the model
    # EMG this is duplicated from above, should be able to combine
    if (model.name == "brt") {
        model.values = matrix(out.model$data$x, ncol=length(out.model$var.names))
        env.vars = out.model$var.names
        colnames(model.values) = env.vars
    } else if (model.name %in% c("geoIDW", "voronoiHull")) {
        model.values = rbind(out.model@presence, out.model@absence)
        env.vars = colnames(model.values)
    } else {
        model.values = out.model@presence
        env.vars = colnames(model.values)
    }

    if (!(length(model.values)==0)) {
        # predict using actual values
        if (model.name == "brt") {
            actual.predictions = predict(out.model, as.data.frame(model.values), n.trees = out.model$gbm.call$best.trees, type = "response")
        } else {
            actual.predictions = predict(out.model, model.values)
        }
        # create a table to hold the output
        varimpt.out = matrix(NA, nrow=length(env.vars), ncol=num_samples+2)
        dimnames(varimpt.out) = list(env.vars, c(paste("sample_", c(1:num_samples, "mean")), "percent"))
        # create a copy of the env data matrix
        sample.data = model.values
        # for each predictor variable
        for (p in 1:ncol(sample.data)) {
            # for each num_sample
            for (s in 1:num_samples) {
                # resample from that variables' values, keeping other variable values the same, and predict suitability
                sample.data[,p] = sample(x=sample.data[,p], replace=FALSE)
                # predict using sampled values
                if (model.name == "brt") {
                    new.predictions = predict(out.model, as.data.frame(sample.data), n.trees = out.model$gbm.call$best.trees, type = "response")
                } else {
                    new.predictions = predict(out.model, sample.data)
                }
                # calculate correlation between original predictions and new predictions
                varimpt.out[p,s] = 1-max(round(cor(x=actual.predictions, y=new.predictions, use="pairwise.complete.obs", method="pearson"), digits=3),0)
            }
        }
        # calculate mean variable importance, normalize to percentages, and write results
        varimpt.out[,num_samples+1] = round(rowMeans(varimpt.out, na.rm=TRUE), digits=3)
        varimpt.out[,num_samples+2] = round((varimpt.out[,num_samples+1]/sum(varimpt.out[,num_samples+1]))*100, digits=0)
        bccvl.write.csv(varimpt.out, name="biomod2_like_VariableImportance.csv")
    } else {
        write(paste(species, ": Cannot calculate variable importance for ", model.name, "object", sep=" "), stdout())
    }
}

# function to calculate variable importance values for dismo models based on Maxent's decrease in AUC
# i.e., hold all but one predictor variable to its original values, resample that one predictor and recalculate model AUC
bccvl.calculatePermutationVarImpt <- function(out.model, model.eval,
                                              model.name, occur, bkgd) {
    # get the enviromental variables and values used to create the model
    # EMG this is duplicated from above, should be able to combine or find an easier way to determine
    if (model.name == "brt") {
        model.values = matrix(out.model$data$x, ncol=length(out.model$var.names))
        env.vars = out.model$var.names
        colnames(model.values) = env.vars
    } else if (model.name %in% c("geoIDW", "voronoiHull")) {
        model.values = rbind(out.model@presence, out.model@absence)
        env.vars = colnames(model.values)
    } else {
        model.values = out.model@presence
        env.vars = colnames(model.values)
    }

    if (!(length(model.values)==0)) {
        # get the occurrence and background environmental data used to evaluate the model
        p.swd=occur
        a.swd=bkgd
        # get the AUC from the original model evaluation
        init.auc = round(model.eval@auc, digits=3)
        # create a table to hold the output
        permvarimpt.out = matrix(NA, nrow=length(env.vars), ncol=4)
        dimnames(permvarimpt.out) = list(env.vars, c("init.auc", "sample.auc", "change.auc", "percent"))
        permvarimpt.out[,"init.auc"] = rep(init.auc, length(env.vars))
        # create a copy of the occurrence and background environmental data
        sample.p = p.swd[,env.vars, drop=FALSE]
        sample.a = a.swd[,env.vars, drop=FALSE]
		# check for and remove any NA's present in the data
		no.na.sample.p = na.omit(sample.p);
                no.na.sample.a = na.omit(sample.a)
		if (nrow(no.na.sample.p) != nrow(sample.p)) {
			write(paste("bccvl.calculatePermutationVarImpt(): NA's were removed from presence data!"), stdout())
		}
		if (nrow(no.na.sample.a) != nrow(sample.a)) {
			write(paste("bccvl.calculatePermutationVarImpt(): NA's were removed from absence data!"), stdout())
		}
        # for each predictor variable
        for (v in 1:length(env.vars)) {
			# resample from that variables' values, keeping other variable values the same
			no.na.sample.p[,v] = sample(x=no.na.sample.p[,v], replace=FALSE)
			no.na.sample.a[,v] = sample(x=no.na.sample.a[,v], replace=FALSE)
            # re-evaluate model with sampled env values
            if (model.name == "brt") {
                sample.eval = dismo::evaluate(p=no.na.sample.p, a=no.na.sample.a, model=out.model, n.trees=out.model$gbm.call$best.trees)
            } else {
                sample.eval = dismo::evaluate(p=no.na.sample.p, a=no.na.sample.a, model=out.model)
            }
            # get the new auc
            permvarimpt.out[v,"sample.auc"] = round(sample.eval@auc, digits=3)
        }
        # calculate the difference in auc, normalize to percentages, and write results
        permvarimpt.out[,"change.auc"] = permvarimpt.out[,"init.auc"] - permvarimpt.out[,"sample.auc"]
        for (r in 1:nrow(permvarimpt.out)) {
            if (permvarimpt.out[r,"change.auc"] < 0) {  # EMG what if AUC increases?
                permvarimpt.out[r,"change.auc"] = 0
            }
        }
        permvarimpt.out[,"percent"] = round((permvarimpt.out[,"change.auc"]/sum(permvarimpt.out[,"change.auc"]))*100, digits=0)
        bccvl.write.csv(permvarimpt.out, name="maxent_like_VariableImportance.csv")
    } else {
        write(paste(species, ": Cannot calculate maxent-like variable importance for ", model.name, "object", sep=" "), stdout())
    }
}

# function to create HTML file with accuracy measures
# need to install and read in the following packages:
#install.packages(c("R2HTML", "png"))
#library(R2HTML)
#library(png)
bccvl.generateHTML <- function() {

    # read in model outputs
    auccurve = readPNG(file.path(bccvl.env$outputdir, "AUC.png"))
    accuracystats <- read.csv(file.path(bccvl.env$outputdir, "combined.modelEvaluation.csv"),
                              row.names=c(1))

    # create the output file
    target = HTMLInitFile(outdir=bccvl.env$outputdir, filename="results", BackGroundColor="#CCCCCC")

    # add content
    HTML("<center><br><H1>Model Output for ", file=target)

    HTML("<br><H2>AUC:ROC curve", file=target)
    HTMLInsertGraph("AUC.png", file=target)

    HTML("<br><H2>Accuracy measures",file=target)
    HTML(accuracystats, file=target)

    # close the file
    HTMLEndFile()
}

###############
#
# evaluate(p, a, model, x, tr, ...)
#
# p presence points (x and y coordinate or SpatialPoints* object)
# Or, if x is missing, values at presence points (EMG: values returned by a predict())
# Or, a matrix with values to compute predictions for
# a absence points (x and y coordinate or SpatialPoints* object)
# Or, if x is missing, values at absence points (EMG: values returned by a predict())
# Or, a matrix with values to compute predictions for
# model any fitted model, including objects inheriting from 'DistModel'; not used when x is missing
# x Optional. Predictor values (object of class Raster*). If present, p and a are interpreted
# as (spatial) points (EMG: lon/lat)
# tr Optional. a vector of threshold values to use for computing the confusion matrices
# ... Additional arguments for the predict function (EMG: evaluate() calls predict())
#
# 'ModelEvaluation' output based on Fielding and Bell (1997) with attributes:
# presence - presence data used
# absence - absence data used
# np - number of presence points
# na - number of absence points
# auc - Area under the receiver operator (ROC) curve
# pauc - p-value for the AUC (for the Wilcoxon test W statistic
# cor - Correlation coefficient
# pcor - p-value for correlation coefficient
# t - vector of thresholds used to compute confusion matrices
# confusion - confusion matrices
# prevalence - Prevalence
# ODP - Overall diagnostic power
# CCR - Correct classification rate
# TPR - True positive rate
# TNR - True negative rate
# FPR - False positive rate
# FNR - False negative rate
# PPP - Positive predictive power
# NPP - Negative predictive power
# MCR - Misclassification rate
# OR - Odds-ratio
# kappa - Cohen's kappa
#
###############

###evaluate the models and save the outputs
bccvl.evaluate.model <- function(model.name, model.obj, occur, bkgd) {
    # evaluate model using dismo's evaluate
    if (model.name == "brt") {
        model.eval = dismo::evaluate(p=occur, a=bkgd, model=model.obj, n.trees=model.obj$gbm.call$best.trees)
    } else {
        model.eval = dismo::evaluate(p=occur, a=bkgd, model=model.obj)
    }
    # need predictions and observed values to create confusion matrices for accuracy statistics
    model.fit = c(model.eval@presence, model.eval@absence)
    model.obs = c(rep(1, length(model.eval@presence)), rep(0, length(model.eval@absence)))

    # get the model accuracy statistics using a modified version of biomod2's Evaluate.models.R
    # TODO: model.accuracy is another global variable
    model.combined.eval = sapply(model.accuracy, function(x){
        return(bccvl.Find.Optim.Stat(Stat=x, Fit=model.fit, Obs=model.obs))
    })
    # save output
    bccvl.saveModelEvaluation(model.eval, model.combined.eval)

    # create response curves
    bccvl.createMarginalResponseCurves(model.obj, model.name)

    # calculate variable importance (like biomod2, using correlations between predictions)
    bccvl.calculateVariableImpt(model.obj, model.name, 3)

    # calculate variable importance (like maxent, using decrease in AUC)
    bccvl.calculatePermutationVarImpt(model.obj, model.eval, model.name, occur, bkgd)

    # create HTML file with accuracy measures
    bccvl.generateHTML()
} # end of evaluate.modol


# function to save evaluate output for BIOMOD2 models
bccvl.saveBIOMODModelEvaluation <- function(loaded.names, biomod.model, use.eval.data=FALSE) {
    # get and save the model evaluation statistics
    # EMG these must specified during model creation with the arg "models.eval.meth"
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

    for ( i in 1:total_models )
    {
        # will be FULL or RUN1 for eg
        model_name = paste(dimnames(predictions)[[3]][i], 
                                    if (use.eval.data) "test" else "training",
                                    sep=".")
        model_predictions = predictions[,,i,,drop=FALSE]

        if (sum(is.na(model_predictions)) == length(model_predictions)) 
        {
            # somewhat adhoc method of determining that the model failed to predict anything
            # Note that we can determine the computed and failed models by inspecting 
            # biomod.model@models.computed and biomod.model@models.failed respectively, however
            # dimnames (model_name) can't be used to match these in a straight forward
            # manner (it may be possible, could go either way here. this way feels simpler)

            # Warn that model n is being ignored. It most probably failed to build.
            warning(sprintf("Warning: Model %i failed to generate. Not generating stats", i), immediate.=T)
            next
        }
        # get the model accuracy statistics using a modified version of biomod2's Evaluate.models.R
        # TODO: model.accuracy is another global variable
        combined.eval = sapply(model.accuracy, function(x){
            return(bccvl.Find.Optim.Stat(Stat = x, Fit = model_predictions, Obs = obs))
        })
        # save all the model accuracy statistics provided in both dismo and biomod2
        rownames(combined.eval) <- c("Testing.data","Cutoff","Sensitivity", "Specificity")
        #bccvl.write.csv(t(round(combined.eval, digits=3)), name="combined.modelEvaluation.csv")
        bccvl.write.csv(t(round(combined.eval, digits=3)), name=paste("combined", model_name, "modelEvaluation.csv", sep="."))

        # save AUC curve
        require(pROC, quietly=T)
        roc1 <- roc(as.numeric(obs), as.numeric(model_predictions), percent=T)
        png(file=file.path(bccvl.env$outputdir, paste("pROC", model_name, "png", sep=".")))
        plot(roc1, main=paste("AUC=",round(auc(roc1)/100,3),sep=""), legacy.axes=TRUE)
        dev.off()
        # save occurence/absence pdfs
        occur_vals=data.frame(vals=roc1$predictor[roc1$response==1])
        absen_vals=data.frame(vals=roc1$predictor[roc1$response==0])
        absen_vals$label="absent"
        occur_vals$label="occur"
        vals=rbind(occur_vals,absen_vals)
        myplot=ggplot(vals, aes(vals, fill=label))  + geom_density(alpha = 0.3)
        myplot = myplot + ggtitle(paste(if (use.eval.data) "Test data:\n" else "Training data:\n",
                                       "Occurrence/absence probability density functions\nbased on model predicted value", 
                                        sep=""))
        ggsave(myplot, filename=paste("occurence_absence_pdf", model_name, "png", sep="."), scale=1.0)
		dev.off()

        myplot=ggplot(vals, aes(vals, fill=label))  + geom_histogram(alpha = 0.5)
        myplot = myplot + ggtitle(paste(if (use.eval.data) "Test data:\n" else "Training data:\n",
                                       "Occurrence/absence histograms\nbased on model predicted value", 
                                        sep=""))
        ggsave(myplot, filename=paste("occurence_absence_hist", model_name, "png", sep="."), scale=1.0)
		dev.off()

        png(file=file.path(bccvl.env$outputdir, paste("true_and_false_posivite_rates", model_name, "png", sep=".")))
        plot(roc1$thresholds, 100-roc1$specificities, type="p", col="blue", xlab="Classification threshold", ylab="Rate")
        par(new=TRUE)
        plot(roc1$thresholds, roc1$sensitivities, type="p", col="red", xlab="", ylab="")
        legend("topright",  title='', legend=c("True positive rate", "False positive rate"), fill=c("red", "blue"), horiz=TRUE)
        title(paste(if (use.eval.data) "Test data:\n" else "Training data:\n",
                    "True and false positive rates according to\nclassification threshold",
                    sep=""))
        dev.off()


        # get and save the variable importance estimates
        variableImpt = get_variables_importance(biomod.model)
        if (!is.na(variableImpt)) {
        #EMG Note this will throw a warning message if variables (array) are returned
            bccvl.write.csv(variableImpt, name=paste("variableImportance", model_name, "txt", sep="."))
        } else {
            message("VarImport argument not specified during model creation!")
            #EMG must create the model with the arg "VarImport" != 0
        }
    }

    # save response curves (Elith et al 2005)
    # TODO: check models parameter ... do I need it? shouldn't it be algo name?
    #       -> would make BIOMOD_LoadMadels call and parameter loaded.name pointless
    for(name in loaded.names)
    {

        png(file=file.path(bccvl.env$outputdir, sprintf("mean_response_curves_%s.png", name)))
        test <- response.plot2(models = name,
                               Data = get_formal_data(biomod.model,"expl.var"),
                               show.variables = get_formal_data(biomod.model,"expl.var.names"),
                               fixed.var.metric = "mean")
         #, data_species = getModelsInputData(biomod.model,"resp.var"))
         # EMG need to investigate why you would want to use this option - uses presence data only
        dev.off()
    }
    return(roc1)
}
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
#biomod.DataSplit = bccvl.params$data_split # default 100; % for calibrating/training, remainder for testing; ignored if DataSplitTable is filled
biomod.DataSplit = 100 # default 100; % for calibrating/training, remainder for testing; ignored if DataSplitTable is filled
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

evaluation = get_evaluations(model.sdm)
bccvl.write.csv(evaluation, name="biomod2.modelEvaluation.csv")
    
#calib.dir=paste(bccvl.env$outputdir, occur.species, ".BIOMOD_DATA", biomod.modeling.id, sep="/")
# output is saved as part of the projection, format specified in arg 'opt.biomod.output.format'
loaded.model = BIOMOD_LoadModels(model.sdm, models=biomod.algorithm)

bccvl.saveBIOMODModelEvaluation(loaded.model, model.sdm, T) # save output
bccvl.saveBIOMODModelEvaluation(loaded.model, model.sdm, F) # save output

