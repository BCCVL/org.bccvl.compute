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

# function to save outputs of new evaluation script (29/01/16)
bccvl.saveNewEvaluation <- function(out.summary, out.performance){
    bccvl.write.csv(data.frame(out.summary), name="evaluation.summary.csv")
    bccvl.write.csv(data.frame(out.performance), name="evaluation.performance.csv")
}

bccvl.saveProjection <- function(proj.model, species) {
  basename = paste("proj", 'current', species, sep="_")
  png(file=file.path(bccvl.env$outputdir, paste(basename, 'png', sep=".")))
  plot(proj.model)
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
bccvl.Find.Optim.Stat <- function(Stat='TSS',Fit,Obs,Precision = 5, Fixed.thresh = NULL){
    uniform_obs=length(unique(Obs)) == 1
    uniform_fit=length(unique(Fit)) == 1
    if ( uniform_obs | uniform_fit ) {
        msg=sprintf("Stat: %s.", Stat)
        if (uniform_obs) msg=sprintf("%s Uniform observed data.", msg)
        if (uniform_fit) msg=sprintf("%s Uniform fitted data.", msg)
        msg=sprintf("%s Be careful with this model's predictions.", msg)
        # warning("\nObserved or fited data contains only a value.. Evaluation Methods switched off\n",immediate.=T)
        # best.stat <- cutoff <- true.pos <- sensibility <- true.neg <- specificity <- NA  
        warning("\nObserved or fited data contains a unique value.. Be carefull with this models predictions\n",immediate.=T)
        #best.stat <- cutoff <- true.pos <- sensibility <- true.neg <- specificity <- NA    
    } #else {
    if(Stat != 'ROC'){
      StatOptimum <- bccvl.getStatOptimValue(Stat)
      if(is.null(Fixed.thresh)){ # test a range of threshold to get the one giving the best score
        if(length(unique(Fit)) == 1){
          valToTest <- unique(Fit)
          valToTest <- round(c(mean(c(0,valToTest)), mean(c(1000,valToTest))))
        } else{
#           mini <- max(min(quantile(Fit,0.05, na.rm=T), na.rm=T),0)
#           maxi <- min(max(quantile(Fit,0.95, na.rm=T), na.rm=T),1000)
          mini <- max(min(Fit, na.rm=T),0)
          maxi <- min(max(Fit, na.rm=T),1000)        
          #valToTest <- unique( round(c(seq(mini,maxi,length.out=100), mini, maxi)) )
          # EMG no idea why the round() is here, it makes vals between 0 and 1 (ie bioclim) all 0
          valToTest <- unique( c(seq(mini,maxi,length.out=100)))
          # deal with unique value to test case
          if(length(valToTest)<3){
            valToTest <- round(c(mean(0,mini), valToTest, mean(1000,maxi)))
          }
        }
#         valToTest <- unique( c(seq(mini,maxi,by=Precision), mini, maxi) )        
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
      true.pos <- as.numeric(misc['TRUE','1'])
      true.neg <- as.numeric(misc['FALSE','0'])
      specificity <- (true.neg * 100)/sum(as.numeric(misc[,'0']))
      sensibility <- (true.pos * 100)/sum(as.numeric(misc[,'1']))
    } else{
#       require(pROC,quietly=T)
      roc1 <- pROC::roc(Obs, Fit, percent=T, direction="<")
      roc1.out <- pROC::coords(roc1, "best", ret=c("threshold", "sens", "spec"))
      best.stat <- as.numeric(pROC::auc(roc1))/100
      cutoff <- as.numeric(roc1.out["threshold"])
      sensibility <- as.numeric(roc1.out["sensitivity"])
      specificity <- as.numeric(roc1.out["specificity"])
    }
  #}
  eval.out <- cbind(best.stat,cutoff,sensibility,specificity)
  rownames(eval.out) <- Stat
  return(eval.out)
}

bccvl.getStatOptimValue <- function(stat){
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

bccvl.calculate.stat <-
function(Misc, stat='TSS')
{
  # Contagency table checking
  Misc <- bccvl.contagency.table.check(Misc)
  
  # Defining Classification index
  hits <- as.numeric(Misc['TRUE','1'])
  misses <- as.numeric(Misc['FALSE','1'])
  false_alarms <- as.numeric(Misc['TRUE','0'])
  correct_negatives <- as.numeric(Misc['FALSE','0'])
  
  total <- sum(as.numeric(Misc))
  forecast_1 <- sum(as.numeric(Misc['TRUE',]))
  forecast_0 <- sum(as.numeric(Misc['FALSE',]))
  observed_1 <- sum(as.numeric(Misc[,'1']))
  observed_0 <- sum(as.numeric(Misc[,'0']))
  
  # Calculating choosen evaluating metric
  if(stat=='TSS'){
    return( (hits/(hits+misses)) + (correct_negatives/(false_alarms+correct_negatives)) -1 )
  }
  
  if(stat=='KAPPA'){
    Po <- (1/total) * (hits + correct_negatives)
    Pe <- ((1/total)^2) * ((forecast_1 * observed_1) + (forecast_0 * observed_0))
    return( (Po - Pe) / (1-Pe) )
  }
  
  if(stat=='ACCURACY'){
    return( (hits + correct_negatives) / total)
  }
  
  if(stat=='BIAS'){
    return( (hits + false_alarms) / (hits + misses))
  }
  
  if(stat=='POD'){
    return( hits / (hits + misses))
  }
  
  if(stat=='FAR'){
    return(false_alarms/(hits+false_alarms))
  }
  
  if(stat=='POFD'){
    return(false_alarms / (correct_negatives + false_alarms))
  }
  
  if(stat=='SR'){
    return(hits / (hits + false_alarms))
  }
  
  if(stat=='CSI'){
    return(hits/(hits+misses+false_alarms))
  }
  
  if(stat=='ETS'){
    hits_rand <- ((hits+misses)*(hits+false_alarms)) / total
    return( (hits-hits_rand) / (hits+misses+false_alarms-hits_rand))
  }
  
  #if(stat=='HK'){
  #  return((hits/(hits+misses)) - (false_alarms/(false_alarms + correct_negatives)))
  #}
  
  #if(stat=='HSS'){
  #  expected_correct_rand <- (1/total) * ( ((hits+misses)*(hits+false_alarms)) +
  #    ((correct_negatives + misses)*(correct_negatives+false_alarms)) )
  #  return((hits+correct_negatives-expected_correct_rand) / (total - expected_correct_rand))
  #}
  
  #if(stat=='OR'){
  #  return((hits*correct_negatives)/(misses*false_alarms))
  #}
  
  #if(stat=='ORSS'){
  #  return((hits*correct_negatives - misses*false_alarms) / (hits*correct_negatives + misses*false_alarms))
  #}
  
  #if(stat=="BOYCE"){
  #  
  #}
  
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

bccvl.contagency.table.check <- function(Misc){
  # Contagency table checking
  if(dim(Misc)[1]==1){
    if(row.names(Misc)[1]=="FALSE"){
      Misc <- rbind(Misc, c(0,0))
      rownames(Misc) <- c('FALSE','TRUE')
    } else{
      a <- Misc
      Misc <- c(0,0)
      Misc <- rbind(Misc, a)
      rownames(Misc) <- c('FALSE','TRUE')
  	}
  }
  
  if(ncol(Misc) != 2 | nrow(Misc) !=2 ){
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
                new.predictions = predict(out.model, as.data.frame(temp.data), n.trees = out.model$gbm.call$best.trees, type="response")
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
            actual.predictions = predict(out.model, as.data.frame(model.values), n.trees = out.model$gbm.call$best.trees, type="response")
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
                sample.eval = dismo::evaluate(p=no.na.sample.p, a=no.na.sample.a, model=out.model, n.trees=out.model$gbm.call$best.trees, type="response")
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
        model.eval = dismo::evaluate(p=occur, a=bkgd, model=model.obj, n.trees=model.obj$gbm.call$best.trees, type="response")
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

    # Call the new evaluation script
    res = performance.2D(model.obs, model.fit, make.plot=model.name, kill.plot=F)
    bccvl.saveNewEvaluation(res$summary, res$performance)

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
bccvl.saveBIOMODModelEvaluation <- function(loaded.names, biomod.model) {
    # get and save the model evaluation statistics
    # EMG these must specified during model creation with the arg "models.eval.meth"
    evaluation = get_evaluations(biomod.model)
    bccvl.write.csv(evaluation, name="biomod2.modelEvaluation.csv")

    # get the model predictions and observed values. predictions is a 4 dimensional array (Predictions, Algorithm, Model run, PseudoAbsence Run)
    predictions = getModelsPrediction(biomod.model)
    total_models = length(dimnames(predictions)[[3]])

    # TODO: get_predictions is buggy; evaluation=FALSE works the wrong way round
    # predictions = get_predictions(biomod.model, evaluation=FALSE)
    obs = get_formal_data(biomod.model, "resp.var")
    # in case of pseudo absences we might have NA values in obs so replace them with 0
    obs = replace(obs, is.na(obs), 0)

    for ( i in 1:total_models )
    {
        model_name = dimnames(predictions)[[3]][i]  # will be FULL or RUN1 for eg
        model_predictions = predictions[,,i,]

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

        res = performance.2D(obs, model_predictions / 1000, make.plot=model_name, kill.plot=F)
        bccvl.saveNewEvaluation(res$summary, res$performance)

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

        png(file=file.path(bccvl.env$outputdir, sprintf("%s-occurence_absence_pdf.png", model_name)), width=480, height=480)
        myplot=ggplot(vals, aes(vals, fill=label)) + geom_density(alpha = 0.3)
        myplot = myplot + ggtitle("Occurrence/absence probability density functions\nbased on model predicted value")
        print(myplot)
        dev.off()

        png(file=file.path(bccvl.env$outputdir, sprintf("%s-occurrence_absence_hist.png", model_name)), width=480, height=480)
        myplot=ggplot(vals, aes(vals, fill=label))  + geom_histogram(alpha = 0.5)
        myplot = myplot + ggtitle("Occurrence/absence histograms\nbased on model predicted value")
        print(myplot)
        dev.off()

        png(file=file.path(bccvl.env$outputdir, sprintf("%s-true_and_false_posivite_rates.png", model_name)), width=480, height=480)        
        plot(roc1$thresholds, 100-roc1$specificities, type="l", col="blue", xlab="Classification threshold", ylab="Rate")
        par(new=TRUE)
        plot(roc1$thresholds, roc1$sensitivities, type="l", col="red", xlab="", ylab="")
        legend("topright",  title='', legend=c("True positive rate", "False positive rate"), fill=c("red", "blue"), horiz=TRUE, bty = "n")
        title("True and false positive rates according to\nclassification threshold")
        dev.off()

        # get and save the variable importance estimates
        variableImpt = get_variables_importance(biomod.model)
        if (!is.na(variableImpt)) {
        #EMG Note this will throw a warning message if variables (array) are returned
            bccvl.write.csv(variableImpt, name=paste("variableImportance", model_name, "csv", sep="."))
        } else {
            message("VarImport argument not specified during model creation!")
            #EMG must create the model with the arg "VarImport" != 0
        }
    }

    # save response curves (Elith et al 2005)
    # TODO: check models parameter ... do I need it? shouldn't it be algo name?
    #       -> would make BIOMOD_LoadMadels call and parameter loaded.name pointless
    #
    # not sure what the comment above means - but ever since we moved to generating
    # output from all models, we could just use biomod.model@models.computed
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
}

absmean <- function(x) abs(mean(x, na.rm=T))
absdiff <- function(x) abs(diff(x, na.rm=T))


performance.2D <- function(obs, pred, make.plot="bccvl", kill.plot=T) {
  library(gridExtra)
  
  # AIM: Calculate 2D measures of predictive performance for any
  # model that predicts a probability of presence (or success), 
  # to be compared to binary observations of presence/absence (or 
  # success/failure).
  #
  # AUTHOR: S.Low-Choy, 2 Jan 2016
  # ACKNOWLEDGEMENTS: Shawn Laffan for useful discussions, 
  # Chantal Huijbers, Sara Richmond and Linda for testcase
  #
  # INPUTS
  # obs = vector of observations of presence/absence (binary)
  # pred = vector of predicted probabilities of presence
  # 
  # OUTPUTS
  # a list with two components,
  # summary - by loss type:
  #  range of threshold ppp for which loss falls within 5% of minimum,
  #  best ppp (corresponding to minimum loss) 
  # performance - measures across varying threshold ppp:
  #  ppp, TPR, FNR, TNR, FPR
  #  loss functions: balanced with FAPP for diagnostic or predictive errors 
  #    in omission; or across select errors: all or diagnostic or predictive. 
  # 
  # TEST: obs <- all$pa; pred <- preds2$fit
  #############################################################
  #
  # MEASURES
  #
  # ERROR CHECKING
  #
  # Check that observations and predictions match length
  nobs <- length(obs)
  if (nobs != length(pred)) stop("Ensure that vectors for observations and predictions are of equal length!")
  
  # Check observations are binary, then recode as a factor if necessary
  tbl.obs <- table(obs)
  diversity.obs <- length(tbl.obs)
  if (diversity.obs==1) stop("All observations are the same!")
  if (diversity.obs!=2) stop("Ensure that the vector of observations only has two possible values.")
  
  # Check coding of presence and absence
  if (is.factor(obs)) {
    truth <- as.character(obs)
    temp <- as.numeric(truth)
    if (!is.na(temp)) 
      truth <- temp 
  } else {
    truth <- obs
  }
  if (is.numeric(truth)) {
    if (any(truth==0)) {
      # presume 0 is coded to be absence
      # if presences are not 1s then recode them to be 1s
      if (any(truth[truth!=0]!=1)) {
        truth[truth!=0] <- 1
      } 
    } else if (any(truth ==1)) {
      # if there are no zeros, then presume 1-2 coding
      if (any(truth ==2)) {
        truth <- truth-1
      }
    } else {
      stop("Can't figure out coding of absence-presence as it is not 0-1 or 1-2. Suggest you recode obs as a factor, with levels 0 and 1")
    } # end if any truth == 0
  } else if (is.character(truth)) {
    # look for "p" for presence, "d" for detected or "o" for occupied
    the.pres <- grep("^[pP]", truth)
    if (any(the.pres)) {
      letter.pres <- "p"
    } else {
      the.pres <- grep("^[Dd]", truth)
      if (any(the.pres)) {
        letter.pres <- "d"
      } else {
        the.pres <- grep("^[oO]", truth)
        if (any(the.pres)) {
          letter.pres <- "o"
        } else {
          stop("Can't figure out coding of presences as they do not start with the letter p, d or o. Suggest you recode presences using one of these options.")
        }
      }
    } # end if any the.pres
    truth[the.pres] <- 1
    
    # recode all non "p" or "d" to be absence (could be "a" for absence, or "n" for not present/seen/detected/occupied, or "u" for unseen etc, or "b" for background)
    tbl.obs <- table(truth)
    letter.abs <- names(tbl.obs)[-grep("1", names(tbl.obs))]
    truth[grep(paste("^",letter.abs,sep=""), truth)] <- 0
    
    if (any(the.pres)) {
      truth <- as.numeric(truth)
    }
  } else {
    stop("Ensure that the data type of observations is numeric, character or factor.")	
  } # end if is.numeric(truth) or is.character(truth)
  
  # Check predictions are probabilities between 0 and 1
  if (any(pred < 0) | any(pred > 1)) stop("Predictions should be probabilities between zero and one. (Check that predictions are not on the log odds scale.)")
  
  # MEASURES
  
  # STEP 1. CALCULATE ERROR MATRICES
  list.ppp <- sort(unique(pred))
  
  fnrs <- tnrs <- fprs <- tprs <- rep(NA, length(list.ppp))
  fns <- tns <- fps <- tps <- rep(NA, length(list.ppp))
  ppvs <- npvs <- fors <- fdrs <- rep(NA, length(list.ppp))
  fpa <- L.bal.diag <- L.bal.pred <- rep(NA, length(list.ppp))
  
  # STEP 2. CALCULATE 1D MEASURES OF PREDICTIVE PERFORMANCE
  for (ell in seq(along=list.ppp)) { 
    # TESTING: ell <- 1; th <- 0.5
    
    th <- list.ppp[ell]
    
    tps[ell] <- length(which(pred>=th & obs==1))
    fps[ell] <- length(which(pred>=th & obs==0))
    tns[ell] <- length(which(pred<th & obs==0))
    fns[ell] <- length(which(pred<th & obs==1))
    
    tprs[ell] <- tps[ell]/(tps[ell]+fns[ell])
    fprs[ell] <- fps[ell]/(fps[ell]+tns[ell])
    tnrs[ell] <- tns[ell]/(tns[ell]+fps[ell])
    fnrs[ell] <- fns[ell]/(fns[ell]+tps[ell])
    # c(tprs[ell],fprs[ell],tnrs[ell],fnrs[ell])
    
    # TPR + FNR = 1, FPR + TNR = 1
    # Typically compare TPR v FPR (or TNR v FNR)
    # and TPR v TNR
    
    # PPV + FDR = 1, NPV + FOR = 1
    # Hence not instructive to compare PPV to FDR (or NPV to FOR)
    # Better to compare PPV v NPV (or FOR v FDR)
    # Also PPV v TPR (or NPV v TNR)
    
    # when predicted present, how many were true presences?
    ppvs[ell] <- tps[ell]/(tps[ell]+fps[ell])
    # when predicted absent, how many were false absences?
    fors[ell] <- fns[ell]/(tns[ell]+fns[ell])
    # when predicted absent, how many were true absences?
    npvs[ell] <- tns[ell]/(tns[ell]+fns[ell])
    # when predicted present, how many were false presences?
    fdrs[ell] <- fps[ell]/(tps[ell]+fps[ell])
    
    # fractional predicted area
    fpa[ell] <- (tps[ell] + fps[ell])/(tps[ell] + fps[ell] + tns[ell] + fns[ell])
    # balance performance metric used in dismo maxent (Hijmans & Elith, 2015)
    # 6 * training omission rate + .04 * cumulative threshold (100%) + 1.6 * fractional predicted area.
    # note rescale of 0.04 to 4 when th is a fraction not a percentage
    L.bal.diag[ell] <- fnrs[ell] * 6 + th*.04 + fpa[ell] * 1.6
    L.bal.pred[ell] <- fors[ell] * 6 + th*.04 + fpa[ell] * 1.6
  }
  
  # compile the information into a dataframe
  temp <- data.frame(list(ppp=list.ppp, tpr=tprs, fpr=fprs,  tnr=tnrs, fnr=fnrs, ppv=ppvs, fdr=fdrs, npv=npvs, fors=fors, fpa=fpa, L.bal.diag=L.bal.diag, L.bal.pred=L.bal.pred))	
  
  # calculate some losses as cost functions across errors
  list.errors <- c("fpr","fnr","fors","fdr")
  temp$L.all <- apply(temp[, list.errors],1, absmean)
  temp$L.diag <- apply(temp[, c("fpr","fnr")],1, absmean)
  temp$L.pred <- apply(temp[, c("fors","fdr")],1, absmean)
  temp$L.pos <- apply(temp[, c("tpr", "ppv")], 1, absmean)
  temp$L.eq.diag <- apply(temp[, c("tpr", "tnr")], 1, absdiff)
  temp$L.eq.pred <- apply(temp[, c("ppv", "npv")], 1, absdiff)
  
  # calculate the best threshold ppp: minimal value of each type of loss
  best <- list(
    bal.diag =temp$ppp[which(temp$L.bal.diag==min(temp$L.bal.diag, na.rm=T))],
    bal.pred =temp$ppp[which(temp$L.bal.pred==min(temp$L.bal.pred, na.rm=T))],
    all =temp$ppp[which(temp$L.all == min(temp$L.all, na.rm=T))], 	
    diag=temp$ppp[which(temp$L.diag == min(temp$L.diag, na.rm=T))],
    pred=temp$ppp[which(temp$L.pred == min(temp$L.pred, na.rm=T))],
    pos= temp$ppp[which(temp$L.pos == max(temp$L.pos, na.rm=T))],
    eq.diag = temp$ppp[which(temp$L.eq.diag==min(temp$L.eq.diag, na.rm=T))],
    eq.pred =temp$ppp[which(temp$L.eq.pred==min(temp$L.eq.pred, na.rm=T))]
  )
  
  # calculate the range of ppp for which each of the losses fall within 5% of the best value
  rangeperf <- matrix(NA, nrow=8, ncol=2, dimnames=list(names(best), c("lower","upper")))
  for (v in names(best)) { # v<-"eq.pred"
    the.v <- paste("L.", v, sep="")
    min.v <- min(temp[,the.v], na.rm=T)
    d.minv <- (abs(temp[,the.v] - min.v) / min.v) 
    the.range <- temp$ppp[ d.minv < 0.05 ]
    rangeperf[dimnames(rangeperf)[[1]]==v, 1:2] <- c(min(the.range, na.rm=T), max(the.range, na.rm=T))
  }
  for (v in c("pos")) { # v<-"pos"
    the.v <- paste("L.", v, sep="")
    max.v <- max(temp[,the.v], na.rm=T)
    d.maxv <- (abs(temp[,the.v] - max.v) / max.v) 
    the.range <- temp$ppp[ d.maxv < 0.05 ]
    rangeperf[dimnames(rangeperf)[[1]]==v, ] <- c(min(the.range, na.rm=T), max(the.range, na.rm=T))		
  }
  rangeperf <- as.data.frame(rangeperf)
  rangeperf$type.of.loss <- names(best)
  rangeperf$best <- unlist(best)
  
  # rescale
  temp$L.bal.diag <- temp$L.bal.diag/max(temp$L.bal.diag, na.rm=T) 
  temp$L.bal.pred <- temp$L.bal.pred/max(temp$L.bal.pred, na.rm=T) 
  temp$L.eq.diag <- temp$L.eq.diag/max(temp$L.eq.diag, na.rm=T) 
  temp$L.eq.pred <- temp$L.eq.pred/max(temp$L.eq.pred, na.rm=T) 
  
  if (make.plot!="") {
    library(reshape2)
    # reshape the data so that it is in long rather than wide format
    errs <- melt(temp, id.var="ppp", measure.var=c("fpr", "fnr", "fdr", "fors", "fpa", "L.bal.diag", "L.bal.pred", "L.all", "L.diag", "L.pred", "L.pos", "L.eq.diag", "L.eq.pred"))
    names(errs)[2] <- c("measure")
    
    # create graph: errors vs different thresholds for predicted probability of presence
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-error.png", make.plot)), width=480, height=480)
    g1 <- ggplot(errs[errs$measure %in% c("fpr", "fnr", "fdr", "fors", "fpa"), ], aes(x=ppp, y=value, colour=measure)) + geom_line() + labs(x="Threshold ppp")
    print(g1)
    dev.off()

    # create graph: show the tradeoffs among errors
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-tradeoffs.png", make.plot)), width=480, height=480)
    g2 <- ggplot(errs[errs$measure %in% rev(c("L.bal.diag", "L.bal.pred", "L.all", "L.diag", "L.pred", "L.pos", "L.eq.diag", "L.eq.pred")), ], aes(x=ppp, y=value, colour=measure)) + geom_line() + labs(x="Threshold ppp") + guides(col=guide_legend(title="Type of Loss, for\nerror tradeoff"))
    print(g2)
    dev.off()

    # add intervals showing within 5%
    rangeperf$type.of.loss <- factor(rangeperf$type.of.loss, levels=(c("bal.diag", "bal.pred", "all", "diag", "pred", "pos", "eq.diag", "eq.pred")))
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-intervals.png", make.plot)), width=480, height=480)
    g3 <- ggplot(rangeperf, aes(x=type.of.loss, y=best, ymin=lower, ymax=upper, colour=type.of.loss)) + geom_pointrange() + coord_flip() + labs(y="Threshold ppp", title="Range of ppp within 5% of minimum per loss") + theme(legend.position="none") + theme(axis.title.x = element_text(hjust = 0))
    print(g3)
    dev.off()

    # ROC 
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-roc.png", make.plot)), width=480, height=480)
    g4 <- ggplot(temp, aes(x=fpr, y=tpr)) + geom_line() + geom_abline(intercept=0, slope=1, colour="grey") + labs(title="ROC", x="Omission rate (FPR)", y="Sensitivity (TPR)")
    print(g4)
    dev.off()

    # density ppp for presence and absence
    temp2 <- data.frame(list(pred=pred, obs=obs))
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-ppp.png", make.plot)), width=480, height=480)
    g5 <- ggplot(temp2, aes(x=pred, colour=factor(obs))) + geom_line(stat="density") + labs(x="ppp", y="Density") + guides(col=guide_legend(title="Observed\ndata")) + theme(legend.position = "top")
    print(g5)
    dev.off()
  }
  
  return(list(summary=rangeperf, performance=temp))	
}

dev.save <- function(fileroot, ext=".pdf") {
  if (ext==".eps") {dev.copy2eps(file=paste(fileroot,ext,sep="."))} else {dev.copy2pdf(file=paste(fileroot,"pdf",sep="."))}
}


########################################################
# Patch Biomod2 plot projection function
#  taken from: biomod2-3.1-64 : https://github.com/cran/biomod2/blob/3.1-64/R/BiomodClass.R#L1586
########################################################
require('rasterVis')
setMethod('plot', signature(x='BIOMOD.projection.out', y="missing"),
          function(x,col=NULL, str.grep=NULL){
            models_selected <- x@models.projected 
            if(length(str.grep)){
              models_selected <- grep(paste(str.grep,collapse="|"), models_selected,value=T)
            } 
            
            if(!length(models_selected)) stop("invalid str.grep arg")
            
            
            
            if(class(x@proj) == "BIOMOD.stored.raster.stack"){
              require(rasterVis)
              
              ## define the breaks of the color key
              my.at <- seq(0,1000,by=100)
              ## the labels will be placed vertically centered
              my.labs.at <- seq(0,1000,by=250)
              ## define the labels
              my.lab <- seq(0,1000,by=250)
              ## define colors
              #               my.col <- colorRampPalette(c("red4","orange4","yellow4","green4"))(100)
              my.col <- colorRampPalette(c("grey90","yellow4","green4"))(100)
              
              ## try to use levelplot function
              try_plot <- try(
                levelplot(get_predictions(x, full.name=models_selected),
                          at=my.at, margin=T, col.regions=my.col,
                          main=paste(x@sp.name,x@proj.names,"projections"),
                          colorkey=list(labels=list(
                            labels=my.lab,
                            at=my.labs.at)))
              ) 
              if(! inherits(try_plot,"try-error")){ ## produce plot
                print(try_plot)
              } else{## try classical plot
                cat("\nrasterVis' levelplot() function failed. Try to call standard raster plotting function.",
                    "It can lead to unooptimal representations.",
                    "You should try to do it by yourself extracting predicions (see : get_predictions() function)", fill=options()$width)
                try_plot <- try(
                  plot(get_predictions(x, full.name=models_selected))
                )
              } 
              
              if(inherits(try_plot,"try-error")){ # try classical plot
                cat("\n Plotting function failed.. You should try to do it by yourself!")
              }
              
            } else if(class(x@proj) == "BIOMOD.stored.array"){
              if(ncol(x@xy.coord) != 2){
                cat("\n ! Impossible to plot projections because xy coordinates are not available !")
              } else {
                multiple.plot(Data = get_predictions(x, full.name=models_selected, as.data.frame=T), coor = x@xy.coord)
              }
              
            } else {cat("\n !  Biomod Projection plotting issue !", fill=.Options$width)}
            
          })
