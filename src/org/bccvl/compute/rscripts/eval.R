### BCCVL SDM model evaluation script ###

### Includes:
# 1: Functions to save output
# 2: Function to evaluate SDM model (written by A/Prof. Sama Low-Choy, Griffith University)
# 3: Functions to create model outputs
# 4: Run the evaluation and save outputs

#########################################################################
### 1: Functions to save SDM outputs
#########################################################################

bccvl.saveModelEvaluation <- function(out.lossfunction, out.evaluation){
  bccvl.write.csv(data.frame(out.lossfunction), name = "Loss function intervals table.csv")
  bccvl.write.csv(data.frame(out.evaluation), name = "Evaluation data.csv")
}

bccvl.saveProjection <- function(proj.model, species) {
  basename = paste("proj", 'current', species, sep="_")
  png(file=file.path(bccvl.env$outputdir, paste(basename, 'png', sep=".")))
  plot(proj.model, on_0_1000=FALSE)
  dev.off()
}

#########################################################################
### 2: Function to evaluate the SDM model
#########################################################################

absmean <- function(x) abs(mean(x, na.rm=T))
absdiff <- function(x) abs(diff(x, na.rm=T))

performance.2D <- function(obs, pred, make.plot="bccvl", kill.plot=T) {
  library(gridExtra)
  
  # AIM: Calculate 2D measures of predictive performance for any
  # model that predicts a probability of presence (or success), 
  # to be compared to binary observations of presence/absence (or 
  # success/failure).
  #
  # AUTHOR: S.Low-Choy, Jan 2016, Griffith University
  # ACKNOWLEDGEMENTS: Shawn Laffan for useful discussions, 
  # Chantal Huijbers, Sarah Richmond and Linda for testcase
  #
  # INPUTS
  # obs = vector of observations of presence/absence (binary)
  # pred = vector of predicted probabilities of presence
  # 
  # OUTPUTS
  # Predicted presence/absence density plot and histogram
  # Sensitivity/Specificity plot
  # ROC plot
  # Plot with four different error rates across varying treshold probability values
  # 4 different loss functions (maximizing TPR+TNR, balancing all errors, or equalising error rates)
  # Table with best probability threshold value corresponding to minimum loss
  # Range of threshold probability values for which loss falls within 5% of minimum
  # 
  #############################################################
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
  
  #
  # MEASURES
  #
  
  # CREATE ERROR MATRICES
  list.tpv <- sort(unique(pred)) 
  
  # tpv = threshold probability value: threshold that is used to transform the continuous probability of presence predicted by the model into a binary prediction:
  # probabilities < tpv = absences, probabilities > tpv = presences
  
  tp <- fp <- tn <- fn <- rep(NA, length(list.tpv))
  tpr <- fpr <- tnr <- fnr <- rep(NA, length(list.tpv))
  ppv <- npv <- fdr <- fors <- rep(NA, length(list.tpv))
  acc <- mcr <- tss <- bs <- csi <- rep(NA, length(list.tpv))
  tp.rand <- ets <- or <- rep(NA, length(list.tpv))
  
  # CALCULATE 1D MEASURES OF PREDICTIVE PERFORMANCE
  
  ## Elements of contigency table:
  
  # tp = True Positives (observed and predicted presences)
  # fp = False Positives (observed absences predicted as presences)
  # tn = True Negatives (observed and predicted absences)
  # fn = False Negatives (observed presences predicted as absences)
  
  for (ell in seq(along=list.tpv)) { 
    
    th <- list.tpv[ell]
    
    tp[ell] <- length(which(pred>=th & obs==1))
    fp[ell] <- length(which(pred>=th & obs==0))
    tn[ell] <- length(which(pred<th & obs==0))
    fn[ell] <- length(which(pred<th & obs==1))
    
  ## Evaluation statistics:
    
    # tpr = True Positive Rate (= Sensitivity) = proportion of observed presences that are correctly predicted.
    tpr[ell] <- tp[ell]/(tp[ell]+fn[ell])
    
    # fpr = False Positive Rate = proportion of observed absences that are incorrectly predicted.
    fpr[ell] <- fp[ell]/(fp[ell]+tn[ell])
    
    # tnr = True Negative Rate (= Specificity) = proportion of observed absences that are correctly predicted.
    tnr[ell] <- tn[ell]/(tn[ell]+fp[ell])
    
    # fnr = False Negative Rate = proportion of observed presences that are incorrectly predicted.
    fnr[ell] <- fn[ell]/(fn[ell]+tp[ell])
    
    # Note: tpr + fnr = 1, tnr + fpr = 1
    
    # ppv = Positive Predictive Value = for all predicted presences, how many were true observed presences?
    ppv[ell] <- tp[ell]/(tp[ell]+fp[ell])
    
    # npv = Negative Predictive Value = for all predicted absences, how many were true absences?
    npv[ell] <- tn[ell]/(tn[ell]+fn[ell])
    
    # fdr = False Discovery Rate = for all predicted presences, how many were false presences (observed absences)?
    fdr[ell] <- fp[ell]/(tp[ell]+fp[ell])
    
    # fors = False Omission Rate = for all predicted absences, how many were false absences (observed presences)?
    fors[ell] <- fn[ell]/(tn[ell]+fn[ell])
    
    # Note: ppv + fdr = 1, npv + for = 1
    
    # acc = Accuracy = proportion of correctly predicted cases.
    acc[ell] <- (tp[ell]+tn[ell])/(tp[ell]+fp[ell]+tn[ell]+fn[ell])
    
    # mcr = Misclassification Rate = proportion of incorrectly predicted cases.
    mcr[ell] <- (fp[ell]+fn[ell])/(tp[ell]+fp[ell]+tn[ell]+fn[ell])
    
    # Note: acc + mcr = 1
    
    # tss = True Skill Statistic - describes how well the model separates presences from absences.
    tss[ell] <- (tpr[ell]-fpr[ell]) 
    
    # bs = Bias Score = frequency of predicted presences compared to the frequency of observed presences.
    bs[ell] <- (tp[ell]+fp[ell])/(tp[ell]+fn[ell])
    
    # csi = Critical Success Index = proportion of observed and predicted presences that are correct.
    csi[ell] <- tp[ell]/(tp[ell]+fp[ell]+fn[ell])
    
    # ets = Equitable Threat Score = proportion of observed and predicted presences that are correct, adjusted for true positives with random chance.
    tp.rand[ell] <- ((tp[ell]+fn[ell])*(tp[ell]+fp[ell]))/(tp[ell]+fp[ell]+tn[ell]+fn[ell])
    ets[ell] <- (tp[ell]-tp.rand[ell])/(tp[ell]+fp[ell]+fn[ell]-tp.rand[ell])  
    
    # or = Odds Ratio = ratio of a correct prediction to an incorrect prediction. 
    or[ell] <- (tp[ell]*tn[ell])/(fp[ell]*fn[ell])
    
  }
  
  # Compile the information into a dataframe
  temp <- data.frame(list(tpv=list.tpv, tpr=tpr, fpr=fpr,  tnr=tnr, fnr=fnr, ppv=ppv, fdr=fdr, npv=npv, fors=fors))   
  
  # CALCULATE 2D MEASURES OF PREDICTIVE PERFORMANCE
  
  # Calculate losses as cost functions across errors
  list.errors <- c("fpr","fnr","fors","fdr")
  
  # Loss functions:
  # L.pos = maximizing the sum of the True Positive Rate (Sensitivity) and the True Negative Rate (Specificity)
  # L.all = balancing all error rates: 1/4(FPR + FNR + FDR + FORS)
  # L.diag = balancing the diagnostic/producer errors: 1/2(FPR + FNR) 
  # L.pred = balancing the predictive/user errors: 1/2(FDR + FORS)
  # L.eq.diag = equalising diagnostic errors: FPR = FNR
  # L.eq.pred = equalising predictive errors: FDR = FORS
  
  temp$L.pos <- apply(temp[, c("tpr", "ppv")], 1, absmean)
  temp$L.all <- apply(temp[, list.errors],1, absmean)
  temp$L.diag <- apply(temp[, c("fpr","fnr")],1, absmean)
  temp$L.pred <- apply(temp[, c("fors","fdr")],1, absmean)
  temp$L.eq.diag <- apply(temp[, c("tpr", "tnr")], 1, absdiff)
  temp$L.eq.pred <- apply(temp[, c("ppv", "npv")], 1, absdiff)
  
  # Calculate the best threshold probability value: maximum for L.pos, minimum value of other loss types
  best <- list(
    pos = temp$tpv[which(temp$L.pos == max(temp$L.pos, na.rm=T))],
    all = temp$tpv[which(temp$L.all == min(temp$L.all, na.rm=T))],   
    diag = temp$tpv[which(temp$L.diag == min(temp$L.diag, na.rm=T))],
    pred = temp$tpv[which(temp$L.pred == min(temp$L.pred, na.rm=T))],
    eq.diag = temp$tpv[which(temp$L.eq.diag==min(temp$L.eq.diag, na.rm=T))],
    eq.pred = temp$tpv[which(temp$L.eq.pred==min(temp$L.eq.pred, na.rm=T))]
  )
  
  # Calculate the range of threshold probability values for which each of the losses fall within 5% of the best value
  rangeperf <- matrix(NA, nrow=6, ncol=2, dimnames=list(names(best), c("lower","upper")))
  for (v in names(best)) { # v<-"eq.pred"
    the.v <- paste("L.", v, sep="")
    min.v <- min(temp[,the.v], na.rm=T)
    d.minv <- (abs(temp[,the.v] - min.v) / min.v) 
    the.range <- temp$tpv[ d.minv < 0.05 ]
    rangeperf[dimnames(rangeperf)[[1]]==v, 1:2] <- c(min(the.range, na.rm=T), max(the.range, na.rm=T))
  }
  for (v in c("pos")) { # v<-"pos"
    the.v <- paste("L.", v, sep="")
    max.v <- max(temp[,the.v], na.rm=T)
    d.maxv <- (abs(temp[,the.v] - max.v) / max.v) 
    the.range <- temp$tpv[ d.maxv < 0.05 ]
    rangeperf[dimnames(rangeperf)[[1]]==v, ] <- c(min(the.range, na.rm=T), max(the.range, na.rm=T))     
  }
  rangeperf <- as.data.frame(rangeperf)
  rangeperf$type.of.loss <- names(best)
  rangeperf$best <- unlist(best)
  
  # Rescale
  temp$L.eq.diag <- temp$L.eq.diag/max(temp$L.eq.diag, na.rm=T) 
  temp$L.eq.pred <- temp$L.eq.pred/max(temp$L.eq.pred, na.rm=T) 
  
  #########################################################################
  ### 3: Functions to create SDM outputs
  #########################################################################
  
  if (make.plot!="") {
    library(reshape2)
    # reshape the data so that it is in long rather than wide format (= each row represents one item, labels are specified by 'measure' column; used by ggplot2)
    errs <- melt(temp, id.var="tpv", measure.var=c("tpr", "tnr", "fpr", "fnr", "fdr", "fors", "L.pos", "L.all", "L.diag", "L.pred", "L.eq.diag", "L.eq.pred"))
    names(errs)[2] <- c("measure")
    
    # Create Presence/absence density plot across threshold probability values
    temp2 <- data.frame(list(pred=pred, obs=obs))
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-presence_absence_plot.png", make.plot)), width=600, height=600)
    g1 <- ggplot(temp2, aes(x=pred, fill=factor(obs))) + 
      geom_density(stat="density", alpha=0.5) + 
      labs(title="Presence/absence density plot \nacross threshold probability values", x="\nThreshold probability value", y="Density\n") + 
      scale_fill_manual(values=c("#EE3B3B", "#6495ED"), labels=c(" Absences      ", " Presences")) +
      theme(axis.text = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)), legend.position="top", legend.key=element_blank(), legend.key.size=unit(1.5, "lines")) + 
      guides(fill=guide_legend(nrow=1, title=NULL))
    print(g1)
    dev.off()
    
    # Create Presence/absence histogram across threshold probability values
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-presence_absence_hist.png", make.plot)), width=600, height=600)
    g2 <- ggplot(temp2, aes(x=pred, fill=factor(obs)))  + 
      geom_histogram(position="dodge", alpha = 0.5) +
      labs(title="Presence/absence histogram \nacross threshold probability values", x="\nThreshold probability value", y="Count\n") +
      scale_fill_manual(values=c("#EE3B3B", "#6495ED"), labels=c(" Absences    ", " Presences")) +
      theme(axis.text = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)), legend.position="top", legend.key=element_blank(), legend.key.size=unit(1.5, "lines")) + 
      guides(fill=guide_legend(nrow=1, title=NULL))
    print(g2)
    dev.off()
    
    # Create TPR-TNR plot
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-TPR-TNR.png", make.plot)), width=600, height=600)
    g3 <- ggplot(errs[errs$measure %in% c("tpr", "tnr"), ], 
                 aes(x=tpv, y=value, colour=measure)) + 
      geom_line(size=1.2) + 
      ylim(0,1) +
      labs(title="Sensitivity-Specificity plot\n", x="\nThreshold probability value", y="TPR/TNR value\n") +
      scale_colour_manual(values=c("#3CAB34", "#049CE3"), labels=c("True Positive Rate (=Sensitivity)", "True Negative Rate (=Specificity)")) + 
      theme(axis.text = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)), legend.position="top", legend.key=element_blank(), legend.key.size=unit(2.5, "lines")) + 
      guides(colour=guide_legend(nrow=2, title=NULL))
    print(g3)
    dev.off()
    
    # Create Error rates plot: shows the values of four different error rates across the range of threshold probability values
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-error-rates.png", make.plot)), width=600, height=600)
    g4 <- ggplot(errs[errs$measure %in% c("fpr", "fnr", "fdr", "fors"), ], 
                 aes(x=tpv, y=value, colour=measure, linetype=measure)) + 
      geom_line(size=1.2) + 
      labs(title="Error rates plot\n", x="\nThreshold probability value", y="Error rate value\n") + 
      scale_linetype_manual(values=c("solid", "solid", "dashed", "dashed"), labels=c("False Positive Rate  ", "False Negative Rate   ", "False Discovery Rate", "False Omission Rate")) +
      scale_colour_manual(values=c("#FAB334", "#D55E00", "#FAB334", "#D55E00"), labels=c("False Positive Rate  ", "False Negative Rate   ", "False Discovery Rate", "False Omission Rate")) + 
      theme(axis.text = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)), legend.position="top", legend.key=element_blank(), legend.key.size=unit(2.5, "lines")) +
      guides(colour=guide_legend(nrow=2, title=NULL), linetype=guide_legend(nrow=2, title=NULL))
    print(g4)
    dev.off()
    
    # Create ROC plot 
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-ROC.png", make.plot)), width=600, height=600)
    g5 <- ggplot(temp, aes(x=fpr, y=tpr)) + 
      geom_line(size=1.2) + 
      geom_abline(intercept=0, slope=1, colour="grey") + 
      labs(x="\nFalse Positive Rate (1-Specificity)", y="True Positive Rate (Sensitivity)\n") +
      ggtitle(paste("ROC plot")) +
      theme(axis.text = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)))
    print(g5)
    dev.off()
    
    # Create Loss function plot: shows the values of different loss functions across the range of threshold probability values
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-loss-functions.png", make.plot)), width=600, height=600)
    g6 <- ggplot(errs[errs$measure %in% rev(c("L.pos", "L.all", "L.eq.diag", "L.eq.pred")), ], 
                 aes(x=tpv, y=value, colour=measure)) + 
      geom_line(size=1.2) + 
      labs(title="Loss function plot\n", x="\nThreshold probability value", y="Loss function value\n") +
      scale_colour_manual(values=c("#48D1CC", "#9F79EE", "#EE9572", "#FF3E96"), labels=c("Maximize TPR + TNR   ", "Balance all errors   ", "FPR = FNR", "FDR = FOR")) + 
      theme(axis.text = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)), legend.position="top", legend.key=element_blank(), legend.key.size=unit(2.5, "lines")) + 
      guides(colour=guide_legend(nrow=2, title = NULL))
    print(g6)
    dev.off()
    
    # Create Loss functions-intervals plot within 5% of the best value
    rangeperf$type.of.loss <- factor(rangeperf$type.of.loss, levels=(c("eq.pred", "eq.diag", "all", "pos")))
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-loss-intervals.png", make.plot)), width=600, height=600)
    g7 <- ggplot(rangeperf, aes(x=type.of.loss, y=best, ymin=lower, ymax=upper, colour=type.of.loss)) + 
      geom_pointrange(size=1.2) + 
      geom_line(size=1.2) +
      coord_flip() + 
      scale_x_discrete(limits=c("eq.pred","eq.diag","all", "pos")) +
      scale_colour_manual(values=c("#48D1CC", "#9F79EE", "#EE9572", "#FF3E96"), labels=c("Maximize TPR + TNR   ", "Balance all errors   ", "FPR = FNR", "FDR = FOR")) + 
      labs(title="Range of threshold probability value \nwithin 5% of minimum per loss\n", x="Type of loss function\n", y="\nThreshold probability value") + 
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text.x = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)), legend.position="top", legend.key=element_blank(), legend.key.size=unit(2.5, "lines")) + 
      guides(colour=guide_legend(nrow=2, title = NULL))
    print(g7)
    dev.off()
    
  }
  
  return(list(summary=rangeperf, performance=temp)) 
}

dev.save <- function(fileroot, ext=".pdf") {
  if (ext==".eps") {dev.copy2eps(file=paste(fileroot,ext,sep="."))} else {dev.copy2pdf(file=paste(fileroot,"pdf",sep="."))}
}

bccvl.createMarginalResponseCurves <- function(out.model, model.name) {
  # Get the enviromental variables and values used to create the model
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
    
    # Create a matrix to hold average values for each environmental variable
    mean.values = matrix(data = NA, nrow = 100, ncol = length(env.vars))
    colnames(mean.values) = env.vars
    # For each variable, populate the column with the mean value
    for (i in 1:ncol(mean.values)) {
      mean.values[,i] = rep(mean(model.values[,i], na.rm=TRUE), 100)
    }
    
    # Allow each environmental variable to vary, keeping other variable values at average, and predict suitability
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
      
      # Create separate file for each response curve
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

#########################################################################
### 4: Run the evaluation and save outputs
#########################################################################

### Evaluation of 'dismo' models and save outputs

bccvl.saveDISMOModelEvaluation <- function(model.name, model.obj, occur, bkgd) {
  # evaluate model using dismo's evaluate
  if (model.name == "brt") {
    model.eval = dismo::evaluate(p=occur, a=bkgd, model=model.obj, n.trees=model.obj$gbm.call$best.trees, type="response")
  } else {
    model.eval = dismo::evaluate(p=occur, a=bkgd, model=model.obj)
  }
  # need predictions and observed values to create confusion matrices for accuracy statistics
  model.fit = c(model.eval@presence, model.eval@absence)
  model.obs = c(rep(1, length(model.eval@presence)), rep(0, length(model.eval@absence)))
  
  # Call the evaluation script
  res = performance.2D(model.obs, model.fit, make.plot=model.name, kill.plot=F)
  bccvl.saveModelEvaluation(res$summary, res$performance)
  
  # Create response curves
  bccvl.createMarginalResponseCurves(model.obj, model.name)
  
  # Calculate variable importance (like biomod2, using correlations between predictions)
  bccvl.calculateVariableImpt(model.obj, model.name, 3)
  
  # Calculate variable importance (like maxent, using decrease in AUC)
  bccvl.calculatePermutationVarImpt(model.obj, model.eval, model.name, occur, bkgd)
  
  # Create HTML file with accuracy measures
  # bccvl.generateHTML()
}

### Evaluation of 'biomod2' models and save outputs

bccvl.saveBIOMODModelEvaluation <- function(loaded.names, biomod.model) {

  evaluation = get_evaluations(biomod.model)

  # Get the model predictions and observed values. Predictions is a 4-dimensional array (Predictions, Algorithm, Model run, PseudoAbsence Run)
  predictions = get_predictions(biomod.model)
  total_models = length(dimnames(predictions)[[3]])
  
  obs = get_formal_data(biomod.model, "resp.var")
  # In case of pseudo-absences we might have NA values in obs so replace them with 0
  obs = replace(obs, is.na(obs), 0)
  
  for ( i in 1:total_models )
  {
    model_name = dimnames(predictions)[[3]][i]  # will be FULL or RUN1 for eg
    model_predictions = predictions[,,i,]
    
    if (sum(is.na(model_predictions)) == length(model_predictions)) 
    {
      # Warn that model n is being ignored. It most probably failed to build.
      warning(sprintf("Warning: Model %i failed to generate. Not generating stats", i), immediate.=T)
      next
    }

    res = performance.2D(obs, model_predictions / 1000, make.plot=model_name, kill.plot=F)
    bccvl.saveModelEvaluation(res$summary, res$performance)
    
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
  for(name in loaded.names)
  {
    png(file=file.path(bccvl.env$outputdir, sprintf("mean_response_curves_%s.png", name)))
    test <- response.plot2(models = name,
                           Data = get_formal_data(biomod.model,"expl.var"),
                           show.variables = get_formal_data(biomod.model,"expl.var.names"),
                           fixed.var.metric = "mean")
    dev.off()
  }
}
