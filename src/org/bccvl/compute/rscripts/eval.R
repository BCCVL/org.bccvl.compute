
######################################################################################
# model accuracy helpers
######################################################################################

# function to save evaluate output
saveModelEvaluation <- function(out.model, out.biomod.model) {
    filename = file.path(outputdir, 'dismo.eval.object.RData')
    # save the 'dismo::ModelEvalution' object
    save(out.model, file=filename)
    # save all the model accuracy statistics provided in both dismo and biomod2
    rownames(out.biomod.model) <- c("Testing.data", "Cutoff", "Sensitivity", "Specificity")
    filename = file.path(outputdir, 'combined.modelEvaluation.csv')
    write.csv(t(round(out.biomod.model, digits=3)), file=filename)
    # EMG no guarantee these value are correct

    # save AUROC curve
    png(file=file.path(outputdir, 'AUC.png'))
    plot(out.model, 'ROC');
    dev.off()
}

my.Find.Optim.Stat <- function(Stat='TSS', Fit, Obs, Precision=5, Fixed.thresh=NULL) {
    if(length(unique(Obs)) == 1 | length(unique(Fit)) == 1){
        # warning("\nObserved or fited data contains only a value.. Evaluation Methods switched off\n",immediate.=T)
        # best.stat <- cutoff <- true.pos <- sensibility <- true.neg <- specificity <- NA
        warning("\nObserved or fited data contains a unique value.. Be carefull with this models predictions\n",immediate.=T)
        #best.stat <- cutoff <- true.pos <- sensibility <- true.neg <- specificity <- NA
    } #else {
    if(Stat != 'ROC'){
        StatOptimum <- my.getStatOptimValue(Stat)
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

        calcStat <- sapply(lapply(valToTest, function(x){return(table(Fit>x,Obs))} ), my.calculate.stat, stat=Stat)

        # scal on 0-1 ladder.. 1 is the best
        calcStat <- 1 - abs(StatOptimum - calcStat)

        best.stat <- max(calcStat, na.rm=T)

        cutoff <- median(valToTest[which(calcStat==best.stat)]) # if several values are selected

        misc <- table(Fit >= cutoff, Obs)
        misc <- .contagency.table.check(misc)
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

my.getStatOptimValue <- function(stat) {
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

my.calculate.stat <- function(Misc, stat='TSS') {
    # Contagency table checking
    Misc <- .contagency.table.check(Misc)

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

.contagency.table.check <- function(Misc) {
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
createMarginalResponseCurves <- function(out.model, model.name) {
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
            range.values = seq(min(model.values[,j]), max(model.values[,j]), length.out=100)
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
            png(file=file.path(outputdir, paste(save.name, "_response.png", sep="")))
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
calculateVariableImpt <- function(out.model, model.name, num_samples) {
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
        write.csv(varimpt.out, file=file.path(outputdir, "biomod2_like_VariableImportance.csv"))
    } else {
        write(paste(species, ": Cannot calculate variable importance for ", model.name, "object", sep=" "), stdout())
    }
}

# function to calculate variable importance values for dismo models based on Maxent's decrease in AUC
# i.e., hold all but one predictor variable to its original values, resample that one predictor and recalculate model AUC
calculatePermutationVarImpt <- function(out.model, model.eval, model.name) {
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
        sample.p = p.swd[,env.vars]
        sample.a = a.swd[,env.vars]
        # for each predictor variable
        for (v in 1:length(env.vars)) {
            # resample from that variables' values, keeping other variable values the same, and
            sample.p[,v] = sample(x=sample.p[,v], replace=FALSE)
            sample.a[,v] = sample(x=sample.a[,v], replace=FALSE)
            # re-evaluate model with sampled env values
            if (model.name == "brt") {
                sample.eval = evaluate(p=sample.p, a=sample.a, model=out.model, n.trees=out.model$gbm.call$best.trees)
            } else {
                sample.eval = evaluate(p=sample.p, a=sample.a, model=out.model)
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
        write.csv(permvarimpt.out, file=file.path(outputdir, "maxent_like_VariableImportance.csv"))
    } else {
        write(paste(species, ": Cannot calculate maxent-like variable importance for ", model.name, "object", sep=" "), stdout())
    }
}

# function to create HTML file with accuracy measures
# need to install and read in the following packages:
#install.packages(c("R2HTML", "png"))
#library(R2HTML) 
#library(png)
generateHTML = function(sp.name) {

	# read in model outputs
	auccurve = readPNG(paste(outputdir, "/AUC.png", sep=""))
	accuracystats = read.csv(paste(outputdir, "/combined.modelEvaluation.csv", sep=""),	row.names=c(1))

	# create the output file 
	target = HTMLInitFile(outdir=outputdir, filename=paste(sp.name,"_output", sep=""), BackGroundColor="#CCCCCC")

		# add content
		HTML(paste("<center><br><H1>Model Output for ", sp.name, sep=""), file=target)

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
evaluate.model <- function(model.name, model.obj, occur, bkgd) {
    # evaluate model using dismo's evaluate
    if (model.name == "brt") {
        model.eval = evaluate(p=occur, a=bkgd, model=model.obj, n.trees=model.obj$gbm.call$best.trees)
    } else {
        model.eval = evaluate(p=occur, a=bkgd, model=model.obj)
    }
    # need predictions and observed values to create confusion matrices for accuracy statistics
    model.fit = c(model.eval@presence, model.eval@absence)
    model.obs = c(rep(1, length(model.eval@presence)), rep(0, length(model.eval@absence)))

    # get the model accuracy statistics using a modified version of biomod2's Evaluate.models.R
    model.combined.eval = sapply(model.accuracy, function(x){
        return(my.Find.Optim.Stat(Stat=x, Fit=model.fit, Obs=model.obs))
    })
    # save output
    saveModelEvaluation(model.eval, model.combined.eval)

    # create response curves
    if (model.name != "brt") { # TODO: doesn't work for brt?
        createMarginalResponseCurves(model.obj, model.name)
    }

    # calculate variable importance (like biomod2, using correlations between predictions)
    calculateVariableImpt(model.obj, model.name, 3)

    # calculate variable importance (like maxent, using decrease in AUC)
    calculatePermutationVarImpt(model.obj, model.eval, model.name)
    
    # create HTML file with accuracy measures
    # TODO -> Fix the species name
	generateHTML("default_species_name")
} # end of evaluate.modol
