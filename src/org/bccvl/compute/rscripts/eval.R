### BCCVL SDM model evaluation script ###

### Includes:
# 1: Functions to save output
# 2: Function to evaluate SDM model (written by A/Prof. Sama Low-Choy, Griffith University)
# 3: Functions to create model outputs
# 4: Run the evaluation and save outputs

#########################################################################
### 1: Functions to save SDM outputs
#########################################################################

bccvl.saveModelEvaluation <- function(out.evaluation, out.stats, out.lossfunction, species_algo_str){
  bccvl.write.csv(data.frame(out.evaluation), 
                  name = paste0(paste("Evaluation-data", species_algo_str, sep="_"), ".csv"))
  bccvl.write.csv(data.frame(out.stats), 
                  name = paste0(paste("Evaluation-statistics", species_algo_str, sep="_"), ".csv"))
  bccvl.write.csv(data.frame(out.lossfunction), 
                  name = paste0(paste("Loss-function-intervals-table", species_algo_str, sep="_"), ".csv"))
  }

bccvl.saveProjection <- function(proj.model, species_algo_str, filename_ext=NULL) {
  if (!is.null(filename_ext)) {
    basename = paste("proj", 'current', species_algo_str, filename_ext, sep="_")
  }
  else {
    basename = paste("proj", 'current', species_algo_str, sep="_")
  }
  png(file=file.path(bccvl.env$outputdir, paste(basename, 'png', sep=".")))
  plot(proj.model, on_0_1000=FALSE)
  dev.off()
}

#########################################################################
### 2: Function to evaluate the SDM model
#########################################################################

absmean <- function(x) abs(mean(x, na.rm=T))
absdiff <- function(x) abs(diff(x, na.rm=T))
pick1min <- function(x) { # w<-c(5,6,7,11,13)
	w <- which(x == min(x, na.rm=T)); 
	len <- length(w); 
	if (len == 1) return(w) else {if (len %% 2 ==1) {return(median(w))} else {return(median(w[-1]))}}
}

performance.2D <- function(obs, pred, species_algo_str, make.plot="bccvl", kill.plot=T) {
  library(gridExtra)
  library(pROC)
  
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
  
  # TESTING 23 July 2018 - SLC - problem with threshold = 0 when predictions only 0 or 1
  # species_algo_str <- "SRE"; make.plot <- "bccvl"; kill.plot <- F; obs<-gobs; pred<-gpred
  
  
  # TESTING 23 July 2018 - SLC - when more than one "best" value as the minimum is achieved twice!
  # Addition Number 2
  
  
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
  
  tbl.pred <- table(pred)
  diversity.pred <- length(tbl.pred)
  if (diversity.pred==1) stop("All predictions are the same!")
  if (any(list.tpv==0)) { if (diversity.pred==2) { list.tpv <- 1; } else { list.tpv <- list.tpv[list.tpv!=0]; } }
  
  # tpv = threshold probability value: threshold that is used to transform the continuous probability of presence predicted by the model into a binary prediction:
  # probabilities < tpv = absences, probabilities > tpv = presences
  
  tp <- fp <- tn <- fn <- rep(NA, length(list.tpv))
  tpr <- fpr <- tnr <- fnr <- rep(NA, length(list.tpv))
  ppv <- npv <- fdr <- fors <- rep(NA, length(list.tpv))
  acc <- mcr <- tss <- bs <- rep(NA, length(list.tpv))
  tp.rand <- ets <- or <- csi <- rep(NA, length(list.tpv))
  Po <- Pe <- kappa <- roc <- auc <- rep(NA, length(list.tpv))
  
  # CALCULATE 1D MEASURES OF PREDICTIVE PERFORMANCE
  
  ## Elements of contigency table:
  
  # tp = True Positives (observed and predicted presences)
  # fp = False Positives (observed absences predicted as presences)
  # tn = True Negatives (observed and predicted absences)
  # fn = False Negatives (observed presences predicted as absences)
  
  for (ell in seq(along=list.tpv)) {  # ell <- 1
    
    th <- list.tpv[ell]
    
    tp[ell] <- length(which(pred>=th & truth==1))
    fp[ell] <- length(which(pred>=th & truth ==0))
    tn[ell] <- length(which(pred<th & truth ==0))
    fn[ell] <- length(which(pred<th & truth ==1))
    
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
    
    # Note: ppv + fdr = 1, npv + fors = 1
    
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
    
    # kappa = Cohen's Kappa = Accuracy of the prediction relative to that of random chance.
    Po[ell] <- acc[ell] # observed accuracy
    Pe[ell] <- ((((tp[ell]+fp[ell])/(tp[ell]+fp[ell]+tn[ell]+fn[ell]))*((tp[ell]+fn[ell])/(tp[ell]+fp[ell]+tn[ell]+fn[ell]))) + (((fn[ell]+tn[ell])/(tp[ell]+fp[ell]+tn[ell]+fn[ell]))*((fp[ell]+tn[ell])/(tp[ell]+fp[ell]+tn[ell]+fn[ell])))) # expected accuracy by random chance
    kappa[ell] <- ((Po[ell]-Pe[ell])/(1-Pe[ell]))
  }

  # auc = Area Under the (ROC) Curve
  roc <- roc(truth, pred)
  auc <- auc(roc)
    
  # Compile the information into dataframes
  temp <- data.frame(list(tpv=list.tpv, tpr=tpr, fpr=fpr,  tnr=tnr, fnr=fnr, ppv=ppv, fdr=fdr, npv=npv, fors=fors))
  auc.d <- data.frame(auc)
  auc.d <- round(auc.d, digits = 2)
  
  # CALCULATE 2D MEASURES OF PREDICTIVE PERFORMANCE
  
  # Calculate losses as cost functions across errors
  list.errors <- c("fpr","fnr","fors","fdr")
  
  # Loss functions:
  # L.diag = minimizing the sum of the diagnostic errors (FPR, FNR)
  #        = maximizing the sum of the True Positive Rate (Sensitivity) and the True Negative Rate (Specificity)
  # L.pred = minimizing the sum of the predictive errors (FDR, FORS)
  #        = maximizing the sum of the Positive Predictive Value and the Negative Predictive Value
  # L.all = balancing all error rates: FPR, FNR, FDR, FORS
  # L.eq.diag = equalising diagnostic errors: FPR = FNR = cross-over of TPR and TNR
  
  temp$L.diag <- apply(temp[, c("fpr","fnr")],1, absmean)
  temp$L.pred <- apply(temp[, c("fors","fdr")],1, absmean)
  temp$L.all <- apply(temp[, list.errors],1, absmean)
  temp$L.eq.diag <- apply(temp[, c("tpr", "tnr")], 1, absdiff)

  # Addition Number 2, 23 July 2018
  # Check if there is more than one minimum, then pick the middle 
  best <- list(
    diag = temp$tpv[pick1min(temp$L.diag) ],
    pred = temp$tpv[pick1min(temp$L.pred == min(temp$L.pred, na.rm=T))],
    all = temp$tpv[pick1min(temp$L.all == min(temp$L.all, na.rm=T))],   
    eq.diag = temp$tpv[pick1min(temp$L.eq.diag==min(temp$L.eq.diag, na.rm=T))]
  )
  
  # End Addition Number 2, 23 July 2018
  
  # Calculate the range of threshold probability values for which each of the losses fall within 5% of the best value
  rangeperf <- matrix(NA, nrow=4, ncol=2, dimnames=list(names(best), c("lower","upper")))
  for (v in names(best)) { # v<-"eq.diag"
    the.v <- paste("L.", v, sep="")
    min.v <- min(temp[,the.v], na.rm=T)
    d.minv <- (abs(temp[,the.v] - min.v) / min.v) 
    the.range <- temp$tpv[ d.minv < 0.05 ]
    rangeperf[dimnames(rangeperf)[[1]]==v, 1:2] <- c(min(the.range, na.rm=T), max(the.range, na.rm=T))
  }
  
  rangeperf <- as.data.frame(rangeperf)
  rangeperf$type.of.loss <- names(best)
  rangeperf$best <- unlist(best)
  
  loss.table <- subset(rangeperf, select = c("lower", "upper", "best"))
  row.names(loss.table) = c("Maximize TPR+TNR", "Maximize PPV+NPV", "Balance all errors", "TPR = TNR")
  loss.table <- as.data.frame(loss.table)
  
  # Rescale
  temp$L.eq.diag <- temp$L.eq.diag/max(temp$L.eq.diag, na.rm=T) 
  

  #########################################################################
  ### 3: Functions to create SDM outputs
  #########################################################################
  
  if (make.plot!="") {
    library(reshape2)
    # reshape the data so that it is in long rather than wide format (= each row represents one item, labels are specified by 'measure' column; used by ggplot2)
    errs <- melt(temp, id.var="tpv", measure.var=c("tpr", "tnr", "fpr", "fnr", "fdr", "fors", "L.diag", "L.pred", "L.all", "L.eq.diag"))
    names(errs)[2] <- c("measure")
    
    # Create Presence/absence density plot across threshold probability values
    temp2 <- data.frame(list(pred=pred, obs=obs))
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-presence-absence-plot_%s.png", make.plot, species_algo_str)), width=480, height=480)
    g1 <- ggplot(temp2, aes(x=pred, fill=factor(obs))) + 
      geom_density(stat="density", alpha=0.5) + 
      labs(title="Presence/absence density plot \nacross predicted probability of presence", x="\nPredicted probability of presence", y="Density\n") + 
      scale_fill_manual(values=c("#EE3B3B", "#6495ED"), labels=c(" Absences      ", " Presences")) +
      theme(axis.text = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)), legend.position="top", legend.key=element_blank(), legend.key.size=unit(1.5, "lines")) + 
      guides(fill=guide_legend(nrow=1, title=NULL))
    print(g1)
    dev.off()
    
    # Create Presence/absence histogram across threshold probability values
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-presence-absence-hist_%s.png", make.plot, species_algo_str)), width=480, height=480)
    g2 <- ggplot(temp2, aes(x=pred, fill=factor(obs)))  + 
      geom_histogram(position="dodge", alpha = 0.5) +
      labs(title="Presence/absence histogram \nacross predicted probability of presence", x="\nPredicted probability of presence", y="Count\n") +
      scale_fill_manual(values=c("#EE3B3B", "#6495ED"), labels=c(" Absences    ", " Presences")) +
      theme(axis.text = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)), legend.position="top", legend.key=element_blank(), legend.key.size=unit(1.5, "lines")) + 
      guides(fill=guide_legend(nrow=1, title=NULL))
    print(g2)
    dev.off()
    
    # Create TPR-TNR plot
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-TPR-TNR_%s.png", make.plot, species_algo_str)), width=480, height=480)
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
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-error-rates_%s.png", make.plot, species_algo_str)), width=480, height=480)
    g4 <- ggplot(errs[errs$measure %in% c("fpr", "fnr", "fdr", "fors"), ], 
                 aes(x=tpv, y=value, colour=measure, linetype=measure)) + 
      geom_line(size=1.2) + 
      ylim(0,1) +
      labs(title="Error rates plot\n", x="\nThreshold probability value", y="Error rate value\n") + 
      scale_linetype_manual(values=c("solid", "solid", "dashed", "dashed"), labels=c("False Positive Rate  ", "False Negative Rate   ", "False Discovery Rate", "False Omission Rate")) +
      scale_colour_manual(values=c("#FAB334", "#D55E00", "#FAB334", "#D55E00"), labels=c("False Positive Rate  ", "False Negative Rate   ", "False Discovery Rate", "False Omission Rate")) + 
      theme(axis.text = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)), legend.position="top", legend.key=element_blank(), legend.key.size=unit(2.5, "lines")) +
      guides(colour=guide_legend(nrow=2, title=NULL), linetype=guide_legend(nrow=2, title=NULL))
    print(g4)
    dev.off()
    
    # Create ROC plot 
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-ROC_%s.png", make.plot, species_algo_str)), width=480, height=480)
    xmax1 = min(round((max(temp$fpr) + 0.2)/0.1)*0.1, 1)
    xpos = max(xmax1/2, 0.1) 
    g5 <- ggplot(temp, aes(x=fpr, y=tpr)) + 
      geom_line(size=1.2) + 
      ylim(0,1) +
      xlim(0, xmax1) +
      geom_abline(intercept=0, slope=1, colour="grey") + 
      labs(x="\nFalse Positive Rate (1-Specificity)", y="True Positive Rate (Sensitivity)\n") +
      ggtitle(paste("ROC plot")) +
      annotate(geom = "text", x = xpos, y = 0.1, label = paste("AUC = ", auc.d$auc), size = 6) +
      theme(axis.text = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)))
    print(g5)
    dev.off()
    
    # Create evaluation stats table with values for optimum tpv value.
    all.stats <- data.frame(list(tpv=list.tpv, tpr=tpr, tnr=tnr, fpr=fpr, fnr=fnr, fdr=fdr, fors=fors, ppv=ppv, npv=npv, kappa=kappa, tss=tss, bs=bs, csi=csi, ets=ets, or=or, acc=acc, mcr=mcr))   
    all.stats <- round(all.stats, digits = 3)
    
    # Addition Number 2, 23 July 2018
    selected_rows = c(pick1min(temp$L.diag), 
                      pick1min(temp$L.pred == min(temp$L.pred, na.rm=T)), 
                      pick1min(temp$L.all == min(temp$L.all, na.rm=T)), 
                      pick1min(temp$L.eq.diag == min(temp$L.eq.diag, na.rm=T)))
    stats.table <- all.stats[selected_rows, ] # select row with all stats for maximum value of loss methods
    rownames(stats.table) <- c("max TPR + TNR", "max PPV + NPV", "balance all errors", "TPR=TNR")
    # End Addition Number 2, 23 July 2018

    # stats.table <- rbind(max.TPR.TNR, TPR.eq.TNR, max.Kappa) 
    names(stats.table) <- c("Optimum threshold value:", "True Positive Rate (TPR)", "True Negative Rate (TNR)", "False Positive Rate (FPR)", "False Negative Rate (FNR)", 
                            "False Discovery Rate (FDR)", "False Omission Rate (FOR)", "Positive Predictive Value (PPV)", "Negative Predictive Value (NPV)", 
                            "Cohen's Kappa", "True Skill Statistic (TSS)", "Bias Score (BS)", "Critical Success Index (CSI)", "Equitable Threat Score (ETS)",
                            "Odds-Ratio (OR)","Accuracy", "Misclassification Rate")
    eval.stats <- t(stats.table) # transpose table

     # Create Loss function plot: shows the values of different loss functions across the range of threshold probability values
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-loss-functions_%s.png", make.plot, species_algo_str)), width=480, height=480)
    g6 <- ggplot(errs[errs$measure %in% rev(c("L.diag", "L.pred", "L.all", "L.eq.diag")), ], 
                 aes(x=tpv, y=value, colour=measure)) + 
      geom_line(size=1.2) + 
      ylim(0,1) +
      labs(title="Loss function plot\n", x="\nThreshold probability value", y="Loss function value\n") +
      scale_colour_manual(values=c("#48D1CC", "#9F79EE", "#EE9572", "#FF3E96"), labels=c("Maximize TPR + TNR   ", "Maximize PPV + NPV   ", "Balance all errors", "TPR = TNR")) + 
      theme(axis.text = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)), legend.position="top", legend.key=element_blank(), legend.key.size=unit(2.5, "lines")) + 
      guides(colour=guide_legend(nrow=2, title = NULL))
    print(g6)
    dev.off()
    
    # Create Loss functions-intervals plot within 5% of the best value
    rangeperf$type.of.loss <- factor(rangeperf$type.of.loss, levels=(c("diag", "pred", "all", "eq.diag")))
    png(file=file.path(bccvl.env$outputdir, sprintf("%s-loss-intervals_%s.png", make.plot, species_algo_str)), width=480, height=480)
    g7 <- ggplot(rangeperf, aes(x=type.of.loss, y=best, ymin=lower, ymax=upper, colour=type.of.loss)) + 
      geom_pointrange(size=1.2) + 
      geom_line(size=1.2) +
      coord_flip() + 
      ylim(0,1) +
      scale_x_discrete(limits=c("diag","pred", "all", "eq.diag")) +
      scale_colour_manual(values=c("#48D1CC", "#9F79EE", "#EE9572", "#FF3E96"), labels=c("Maximize TPR + TNR   ", "Maximize PPV + NPV   ", "Balance all errors   ", "TPR = TNR")) + 
      labs(title="Range of threshold probability value \nwithin 5% of minimum per loss\n", x="Type of loss function\n", y="\nThreshold probability value") + 
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text.x = element_text(family="Arial", size=rel(1.5)), axis.title = element_text(family="Arial", size=rel(1.5)), plot.title = element_text(family="Arial", size=rel(2)), legend.text = element_text(family="Arial", size=rel(1.5)), legend.position="top", legend.key=element_blank(), legend.key.size=unit(2.5, "lines")) + 
      guides(colour=guide_legend(nrow=2, title = NULL))
    print(g7)
    dev.off()
  }
  
  return(list(performance=temp, stats=eval.stats, loss.summary=loss.table)) 
}

dev.save <- function(fileroot, ext=".pdf") {
  if (ext==".eps") {dev.copy2eps(file=paste(fileroot,ext,sep="."))} else {dev.copy2pdf(file=paste(fileroot,"pdf",sep="."))}
}

bccvl.createMarginalResponseCurves <- function(out.model, model.name, species_algo_str) {
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

    # plot 18 response curves per page
    curvesPerPage = 6*3       # No of rows X No of columns
    for (i in 0:((ncol(mean.values)-1)/curvesPerPage)) {
        png(file=file.path(bccvl.env$outputdir, sprintf("response_curve_%s_p%d.png", species_algo_str, i)), width=700, height=900)
        par(mfrow = c(6,3)) # No of rows X No of columns

        # Allow each environmental variable to vary, keeping other variable values at average, and predict suitability
        rcurves = list()
        for (j in ((i*curvesPerPage + 1):min((i+1)*curvesPerPage, ncol(mean.values)))) {
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
            plot(range.values, new.predictions, ylim=c(0,1), xlab="", ylab="", main=save.name, type="l")
            rug(model.values[,j])

            # save the response curve for later use
            df1 = data.frame(range.values, new.predictions)
            names(df1) <- c(save.name, "")
            rcurves[[save.name]] = df1
        }
        dev.off()

        # Save each response curve 
        for (k in 1:length(rcurves))
        {
          ename = env.vars[k + i*curvesPerPage]
          png(file=file.path(bccvl.env$outputdir, sprintf("%s_response_curve_%s.png", ename, species_algo_str)))
          plot(rcurves[[ename]], ylim=c(0,1), xlab="", ylab="", main=ename, type="l")
          rug(model.values[, k + i*curvesPerPage])
          dev.off()
        }
        rcurves = NULL
    }
  } else {
    write(paste(species_algo_str, ": Cannot create response curves from", model.name, "object", sep=" "), stdout())
  }
}

# function to calculate variable importance values for dismo models based on biomod2's correlation between predictions
# i.e., hold all but one predictor variable to its actual values, resample that one predictor and recalculate model predictions
bccvl.calculateVariableImpt <- function(out.model, model.name, num_samples, species_algo_str) {
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
    bccvl.write.csv(varimpt.out, name=sprintf("biomod2_like_VariableImportance_%s.csv", species_algo_str))
  } else {
    write(paste(species, ": Cannot calculate variable importance for ", model.name, "object", sep=" "), stdout())
  }
}

# function to calculate variable importance values for dismo models based on Maxent's decrease in AUC
# i.e., hold all but one predictor variable to its original values, resample that one predictor and recalculate model AUC
bccvl.calculatePermutationVarImpt <- function(out.model, model.eval,
                                              model.name, occur, bkgd, species_algo_str) {
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
    bccvl.write.csv(permvarimpt.out, name=sprintf("maxent_like_VariableImportance_%s.csv", species_algo_str))
  } else {
    write(paste(species, ": Cannot calculate maxent-like variable importance for ", model.name, "object", sep=" "), stdout())
  }
}

#########################################################################
### 4: Run the evaluation and save outputs
#########################################################################

### Evaluation of 'dismo' models and save outputs

bccvl.saveDISMOModelEvaluation <- function(model.name, model.obj, occur, bkgd, species.name) {
  species_algo_str = paste(species.name, model.name, sep="_")

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
  res = performance.2D(model.obs, model.fit, species_algo_str, make.plot="dismo", kill.plot=F)
  bccvl.saveModelEvaluation(res$performance, res$stats, res$loss.summary, species_algo_str)
  
  # Create response curves
  bccvl.createMarginalResponseCurves(model.obj, model.name, species_algo_str)
  
  # Calculate variable importance (like biomod2, using correlations between predictions)
  bccvl.calculateVariableImpt(model.obj, model.name, 3, species_algo_str)
  
  # Calculate variable importance (like maxent, using decrease in AUC)
  bccvl.calculatePermutationVarImpt(model.obj, model.eval, model.name, occur, bkgd, species_algo_str)
  
  # Create HTML file with accuracy measures
  # bccvl.generateHTML()
}

### Evaluation of 'biomod2' models and save outputs

bccvl.saveBIOMODModelEvaluation <- function(loaded.names, biomod.model, species_algo_str) {

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

    res = performance.2D(obs, model_predictions / 1000, species_algo_str, make.plot=model_name, kill.plot=F)
    bccvl.saveModelEvaluation(res$performance, res$stats, res$loss.summary, species_algo_str)
    
    # get and save the variable importance estimates
    variableImpt = get_variables_importance(biomod.model)
    if (!is.na(variableImpt)) {
      #EMG Note this will throw a warning message if variables (array) are returned
      bccvl.write.csv(variableImpt, 
                      name=paste("variableImportance", model_name, species_algo_str, "csv", sep="."))
    } else {
      message("VarImport argument not specified during model creation!")
      #EMG must create the model with the arg "VarImport" != 0
    }
  }
  
  # save response curves (Elith et al 2005)
  for(name in loaded.names)
  {
    env_data = get_formal_data(biomod.model,"expl.var")
    png(file=file.path(bccvl.env$outputdir, sprintf("mean_response_curves_%s.png", name)))
    test <- response.plot2(models = name,
                           Data = env_data,
                           show.variables = get_formal_data(biomod.model,"expl.var.names"),
                           fixed.var.metric = "mean")
    dev.off()

    # save individual response curves
    for (envname in names(test))
    {
      png(file=file.path(bccvl.env$outputdir, sprintf("%s_mean_response_curves_%s.png", envname, name)))
      plot(test[[envname]], type='l', ylim=c(0, 1.0), main=envname, xlab="", ylab="")
      rug(env_data[[envname]])
      dev.off()
    }
  }
}

#
# ------------------------------------------
#

bccvl.savePdf <- function(..., filename, aspdf, outputdir=bccvl.env$outputdir)
{
  library("gridExtra")
  if (aspdf) 
  { 
    png(file=file.path(outputdir, paste(filename, 'png', sep=".")))
    grid.arrange(...)
    dev.off()  
  }
  else {     
    grid.arrange(...)
  }
}


# A special R function for variable importance plots based on biomod2 fitted model outcomes

# TODO: data1 passed in here may encode pseudo absence as NA ... we should convert that to 0
# TODO: could use checks for covars != NA in data1
bccvl.VIPplot <- function(fittedmodel=NULL, 
                         method=c("glm",,"cta","gam","ann", "rf", "gbm", "mars", "maxent"),  
                         cor.method=c("pearson","spearman"),
                         pdf=TRUE, biom_vi=FALSE,output.table=FALSE, data1, this.dir, filename)
{
  library("ggplot2")
  library("reshape2")
  library("mgcv")
  library("rpart")
  library("caret")
  library("ggdendro")

  # README notes:
  # (1) fittedmodel: the fitted model object obtained running the biomod2 function'BIOMOD_Modeling'.
  #     method is one of "glm","rpart","gam","ann", "rf", "gbm", "mars", "maxent.p"
  #     cor.method is one of "pearson","spearman"
  # (2) pdf=FALSE: the default setting - no pdf file generated;  pdf=TRUE: the VIP generated is
  #     saved as a pdf file in the working directory.
  # (3) biom_vi=FALSE: a function/algorithm other than the biomod2 inbuilt function 'variables_importance' 
  #     will be applied for evaluating/ranking the variable importance.
  # (4) output.table=FALSE: a .csv file which contains a table to display the glm model parameter
  #     estimates and the 95% confidence bounds for both raw data and scaled data will be generated
  #     if output.table=TRUE.
  # (5) cor.method=c("pearson","spearman"): the default "pearson" method measures only the linear 
  #     association among variables; the "spearman" method is a rank-based algorithm which is more
  #     robust measure for association among variables, e.g., the non-linear association will also be detected.
  # (6) Note that 'data1' is a dataframe with the response variable in the first column following
  #     by the predictor variables / enviornmental variables in the other columns;
  #     'data1' is needed for generating the VIP by the biomod2 inbuilt function
  #     'variables_importance(fittedmodel, data1)' and for calculate the AIC scores.
  #     Warning: Except for the glm algorithm, the 'data1' should include all predictor variables to make
  #     the variable importance ranking outcomes meaningful.
  # (7) this.dir specifies the route to access the biomod2 model (but not including the model name)
  # (8) filename to be saved without the file extension.
  # 

  data1$y.data[is.na(data1$y.data)] <- 0
  
 # extract the root of filenames used by biomod to save model results
 filenames <- dir(this.dir)
 #loc <- regexpr("_RUN[[:digit:]]", filenames[1])
 #fileroot <- substr(filenames[1], 1, loc-1)

# select the full model generated
 filekeep <-  paste(this.dir, "/", filenames[1], sep="")
 
 if (!is.na(match("glm",method))) 
 {
   working <- load(filekeep)
   fittedmodel <- get_formal_model(eval(parse(text=working)))

   fitteddata = fittedmodel$data  # these are the data used by biomod2 for model fitting
   nd = dim(fitteddata)[2]
   sub.data = fitteddata[,2:nd, drop=FALSE]
   # Can only scale numeric data
   cat.data = Filter(is.factor, sub.data)
   sub.data = Filter(is.numeric, sub.data)
   RespV = fitteddata[,1, drop=FALSE]
   rescaled.data <- scale(sub.data)

   # attributes(rescaled.data)
   all.data <- cbind(RespV, as.data.frame(rescaled.data), cat.data)

   # head(all.data); dim(all.data)
   rescaled.glm <- glm(fittedmodel$formula, data=all.data, family=binomial)

   scaled = as.data.frame(cbind(coef(rescaled.glm), confint(rescaled.glm)))
   scaled = scaled[-1,]
   raw = as.data.frame(cbind(coef(fittedmodel), confint(fittedmodel)))
   raw = raw[-1,]

   #  variable importance plot in terms of relative effect size
   #  the relative effect size is defined as the coefficient estimated based on scaled data
   nx = length(coef(fittedmodel)[-1])
   df1 = as.data.frame(cbind(1:nx,round(scaled,3)))

   names(df1) = c("xvar", "meanest", "lower", "upper")
   df1$xvar = factor(df1$xvar, labels = rownames(df1))

   p1 <- ggplot(df1, aes(x=xvar, y=meanest)) + geom_hline(yintercept = 0) + labs(x=" ")

   ps = p1 + geom_errorbar(aes(ymin=lower,ymax=upper),lwd=0.8,width=0.25) + 
      labs(y="relative effect size") +  labs(title="         scaled data") + coord_flip()

   df2 = as.data.frame(cbind(1:nx,round(raw,3)))
   names(df2) = c("xvar", "meanest", "lower", "upper")
   df2$xvar = factor(df2$xvar, labels = rownames(df2))

   if (output.table) 
   {
      df1t = df1; df2t = df2
      names(df1t) = c("x.var", "coeff.est.scaled", "lower", "upper")
      names(df2t) = c("x.var", "coeff.est.raw", "lower", "upper")
      dfout = cbind(df2t,df1t)
      write.csv(dfout,file=paste(filekeep,"paraest_out.csv",sep="_"),row.names=FALSE)
   }

   #  the heatmap in terms of correlation among numerical predictor variables
   rescdata = Filter(is.numeric, rescaled.glm$data[,-1, drop=FALSE])

   if("spearman" %in% cor.method) {
       xx = cor(rescdata, method="spearman")
   } else if("pearson" %in% cor.method)  {
       xx = cor(rescdata)
   }

   lower_tri <- xx
   lower_tri[upper.tri(lower_tri)] <- NA
  
   xx.ml <- melt(lower_tri,na.rm=TRUE)  #the argument 'na.rm=TRUE' seems not working)

   corx = xx.ml[,3]
   rm = which(is.na(corx)==TRUE)
   xx.ml = xx.ml[-rm,]

   pheat <- ggplot(xx.ml, aes(X1, X2)) + geom_tile(aes(fill = value), colour="black") + 
      scale_fill_gradient2(low = "green4", high = "violetred", mid="white", 
      midpoint=0, limit=c(-1,1)) + labs(y=" ") + theme_minimal() +
      scale_x_discrete(limits=rownames(xx)) + scale_y_discrete(limits=colnames(xx)) + coord_fixed() +
      theme(axis.title.x=element_blank(),legend.position = "bottom", axis.text.x=element_text(angle=-90)) +
      guides(fill=guide_legend(title="correlation"))

   # Save as variable correlation plot.
   filename1 = sub("vip_plot", "variable_correlations", filename)
   bccvl.savePdf(pheat, ncol=1, nrow=1, filename=filename1, aspdf=pdf)

   # variable importance plot in terms of AIC scores which represent the information loss,
   # e.g., the AIC score of a predictor variable representing the information loss 
   # if this variable is not included in the selected model.

   nd = dim(data1)[2]

   RespV1 = data1[,1]; subdata1 = data1[,2:nd, drop=FALSE]
   glm.all = glm(formula = RespV1 ~ ., family = binomial, data = subdata1)

   Xaic = NULL
   for (i in 1:(nd-1))
   {
      subdf = subdata1[,-i, drop=FALSE]
      glm.one = glm(formula = RespV1 ~ . , family = binomial, data = subdf)
      Xaic = c(Xaic,AIC(glm.one)) 
   }

   relaAIC = round(Xaic - AIC(glm.all),2)  
   nx = length(relaAIC)
   dfa = as.data.frame(cbind(1:nx,relaAIC))
   dfa$V1 = factor(dfa$V1, labels = rev(names(subdata1)))
   pa <- ggplot(dfa, aes(x=V1, y=rev(relaAIC))) + labs(x="predictor variables") + 
   labs(y="AIC score for information loss") + labs(title="AIC approach")
  
   ppa = pa + geom_col(alpha=0.6,col="blue") + coord_flip()

   # the variable importance plot using the inbuilt biomod2 function 'variables_importance'
   vi_biomod = variables_importance(fittedmodel,data=subdata1)$mat
   nx = length(vi_biomod)
   dfvi = as.data.frame(cbind(1:nx,vi_biomod[,1]))
   dfvi$V1 = factor(dfvi$V1, labels = rev(rownames(vi_biomod)))
   pv <- ggplot(dfvi, aes(x=V1, y=rev(vi_biomod[,1]))) + labs(x="predictor variables") + 
      labs(y="variable importance score") + labs(title="biomod2 function 'variables_importance'")
 
   ppv = pv + geom_col(alpha=0.6,col="green4") + coord_flip()

   # Save as variable relative contribution plot.
   filename1 = sub("vip_plot", "variable_relative_contribution", filename)
   if (biom_vi) {
     bccvl.savePdf(ps, ppv, ncol=2, nrow=1, filename=filename1, aspdf=pdf)
   }
   else {
     bccvl.savePdf(ps, ppa, ncol=2, nrow=1, filename=filename1, aspdf=pdf)
   }
 }

 if (!is.na(match("cta",method))) 
 {
   working <- load(filekeep)
   fittedmodel <- get_formal_model(eval(parse(text=working)))

# variable importance is part of the model fitting outcomes with 'rpart' algorithm and
# this information can be used for generating the variable importance plot

   varimp0 = fittedmodel$variable.importance
   nx = length(varimp0)
   df0 = as.data.frame(cbind(1:nx,varimp0))
   df0$V1 = factor(df0$V1, labels = rev(names(varimp0)))

   p <- ggplot(df0, aes(x=V1, y=rev(varimp0))) + labs(x=" ") + 
      labs(y="variable importance score") + labs(title="part of the 'rpart' model output")

   pp0 = p + geom_col(alpha=0.6,col="blue") + coord_flip()

   ddata <- dendro_data(fittedmodel)
   ppt = ggplot() + 
      geom_segment(data = ddata$segments, aes(x = x, y = y, xend = xend, yend = yend)) + 
      geom_text(data = ddata$labels, aes(x = x, y = y, label = label), size = 3, vjust = 0) +
      geom_text(data = ddata$leaf_labels, aes(x = x, y = y, label = label), size = 3, vjust = 1) +
      theme_dendro()

   # variable importance plot using the inbuilt biomod2 function 'variables_importance'

   nd = dim(data1)[2]
   subdata1 = data1[,2:nd, drop=FALSE]
   vi_biomod = variables_importance(fittedmodel,data=subdata1)$mat
   nx = length(vi_biomod)
   dfvi = as.data.frame(cbind(1:nx,vi_biomod[,1]))
   dfvi$V1 = factor(dfvi$V1, labels = rev(rownames(vi_biomod)))
   pv <- ggplot(dfvi, aes(x=V1, y=rev(vi_biomod[,1]))) + labs(x="predictor variables") + 
      labs(y="variable importance score") + labs(title="biomod2 function 'variables_importance'")
   ppv = pv + geom_col(alpha=0.6,col="green4") + coord_flip()

   if (biom_vi) 
   {
     bccvl.savePdf(ppt, ppv, ncol=2, nrow=1, filename=filename, aspdf=pdf)
   }
   else
   {
     bccvl.savePdf(ppt, pp0, ncol=2, nrow=1, filename=filename, aspdf=pdf)
   }
 }


 if (!is.na(match("gam",method))) 
 {
   working <- load(filekeep)
   fittedmodel <- get_formal_model(eval(parse(text=working)))

   if (biom_vi) 
   {
     # variable importance plot using the inbuilt biomod2 function 'variables_importance'
     nd = dim(data1)[2]  
     RespV1 = data1[,1]; subdata1 = data1[,2:nd, drop=FALSE]
   
     vi_biomod = variables_importance(fittedmodel,data=subdata1)$mat
     nx = length(vi_biomod)
     dfvi = as.data.frame(cbind(1:nx,vi_biomod[,1]))
     dfvi$V1 = factor(dfvi$V1, labels = rev(rownames(vi_biomod)))
     pv <- ggplot(dfvi, aes(x=V1, y=rev(vi_biomod[,1]))) + labs(x="predictor variables") + 
        labs(y="variable importance score") + labs(title="biomod2 function 'variables_importance'")
     ppv = pv + geom_col(alpha=0.6,col="green4") + coord_flip()
    
     bccvl.savePdf(ppV, ncol=1, nrow=1, filename=filename, aspdf=pdf)
   }  # end of 'if(biom_vi=TRUE)' 
   else 
   {
     # variable importance plot following the AIC approach
     nd = dim(data1)[2]  
     RespV1 = data1[,1]
     subdata1 = data1[,2:nd, drop=FALSE]

     # gam function cannot take categorical data, so exclude categorical data.
     subdata1 = Filter(is.numeric, subdata1)

     xname = names(subdata1)
     sname = paste("s(", xname, ")",sep="")

     gamformu.all <- as.formula(paste("RespV1 ~ 1 +", paste(sname, collapse= "+")))
     gam.all = gam(formula = gamformu.all, family = binomial, data = subdata1)

     Xaic = NULL
     nd = dim(subdata1)[2]
     for (i in 1:nd)
     {
        subdf = subdata1[, -i, drop=FALSE]
        xname1 = names(subdf)
        sname1 = paste("s(", xname1, ")",sep="")
        gamformu1 <- as.formula(paste("RespV1 ~ 1 +", paste(sname1, collapse= "+")))
        gam.one = gam(formula = gamformu1, family = binomial, data = subdf)
        Xaic = c(Xaic,AIC(gam.one)) 
     }

     relaAIC = round(Xaic - AIC(gam.all),2)
     nx = length(relaAIC)
     dfa = as.data.frame(cbind(1:nx,relaAIC))
     dfa$V1 = factor(dfa$V1, labels = rev(names(subdata1)))
     pa <- ggplot(dfa, aes(x=V1, y=rev(relaAIC))) + labs(x="predictor variables") + 
        labs(y="AIC score for information loss") + labs(title="AIC approach")
    
     ppa = pa + geom_col(alpha=0.6,col="blue") + coord_flip()

     bccvl.savePdf(ppa, ncol=1, nrow=1, filename=filename, aspdf=pdf)

   }
 }

 if (!is.na(match("ann",method))) 
 {
   working <- load(filekeep)
   fittedmodel <- get_formal_model(eval(parse(text=working)))

   # variable importance plot using the inbuilt biomod2 function 'variables_importance'
   nd = dim(data1)[2]  
   RespV1 = data1[,1]; subdata1 = data1[,2:nd]
 
   vi_biomod = variables_importance(fittedmodel,data=subdata1)$mat
   nx = length(vi_biomod)
   dfvi = as.data.frame(cbind(1:nx,vi_biomod[,1]))
   dfvi$V1 = factor(dfvi$V1, labels = rev(rownames(vi_biomod)))
   pv <- ggplot(dfvi, aes(x=V1, y=rev(vi_biomod[,1]))) + labs(x="predictor variables") + 
      labs(y="variable importance score") + labs(title="biomod2 function 'variables_importance'")
   ppv = pv + geom_col(alpha=0.6,col="green4") + coord_flip()
  
   bccvl.savePdf(ppv, ncol=1, nrow=1, filename=filename, aspdf=pdf)
 }

 if (!is.na(match("mars",method))) 
 {
   # Note that the model class generated from MARS algorithm is not supported by 
   #  the inbuilt biomod2 function 'variables_importance', neither the AIC approach is applicable.
   # However, the function 'varImp' in package 'caret' accept the MARS model object for estimating
   #  the variable importance. GCV = generalized cross validation
   working <- load(filekeep)
   fittedmodel <- get_formal_model(eval(parse(text=working)))

   # variable importance plot using the inbuilt function 'varImp' from package 'caret'
   nd = dim(data1)[2]  
   RespV1 = data1[,1]; subdata1 = data1[,2:nd]
 
   var_imp = varImp(fittedmodel)
   nx = length(var_imp[,1])
   dfvi = as.data.frame(cbind(1:nx,var_imp[,1]))
   dfvi$V1 = factor(dfvi$V1, labels = rev(rownames(var_imp)))
   pv <- ggplot(dfvi, aes(x=V1, y=rev(var_imp[,1]))) + labs(x="predictor variables") + 
      labs(y="relative reduction in GCV") + labs(title="function 'varImp' in package 'caret'")
   ppv = pv + geom_col(alpha=0.6,col="red") + coord_flip()
  
   bccvl.savePdf(ppv, ncol=1, nrow=1, filename=filename, aspdf=pdf)
 }
   
  
 if (!is.na(match("gbm",method))) 
 {
   working <- load(filekeep)
   fittedmodel <- get_formal_model(eval(parse(text=working)))
 
   # variable importance plot using the inbuilt biomod2 function 'variables_importance'
   nd = dim(data1)[2]  
   RespV1 = data1[,1]; subdata1 = data1[,2:nd]
 
   vi_biomod = variables_importance(fittedmodel,data=subdata1)$mat
   nx = length(vi_biomod)
   dfvi = as.data.frame(cbind(1:nx,vi_biomod[,1]))
   dfvi$V1 = factor(dfvi$V1, labels = rev(rownames(vi_biomod)))
   pv <- ggplot(dfvi, aes(x=V1, y=rev(vi_biomod[,1]))) + labs(x="predictor variables") + 
      labs(y="variable importance score") + labs(title="biomod2 function 'variables_importance'")
   ppv = pv + geom_col(alpha=0.6,col="green4") + coord_flip()
  
   bccvl.savePdf(ppv, ncol=1, nrow=1, filename=filename, aspdf=pdf)
 }


 if (!is.na(match("rf",method))) 
 {
   working <- load(filekeep)
   fittedmodel <- get_formal_model(eval(parse(text=working)))
 
   # Random forests (rf) provide an improvement over bagged trees by way of a small tweak
   #  that decorrelates the trees.
   # Note that the variable importance plot using the inbuilt biomod2 function 'variables_importance'
   #  does not seem working with Random forests algorithm.  On the other hand, however, the fitted
   #  rf model object contains the variable importance information which is measured by the mean derease
   #  in Gini index (expressed relative to the maximum).  
   # While RSS is used for measuring the regression tree model performance, the Gini index is used for 
   #  measuring the classification tree model performance and Gini index is a measure of total variance
   #  across the K classes.

   nd = dim(data1)[2]  
   RespV1 = data1[,1]; subdata1 = data1[,2:nd]
 
   out.rf = fittedmodel$importance
   rfImp = out.rf[,1]
   nx = length(rfImp)

   dfrf = as.data.frame(cbind(1:nx,rfImp))
   dfrf$V1 = factor(dfrf$V1, labels = rev(names(rfImp)))
   prf <- ggplot(dfrf, aes(x=V1, y=rev(rfImp))) + labs(x="predictor variables") + 
       labs(y="mean decrease in Gini index") + labs(title="part of rf model fitting outputs")
   pprf = prf + geom_col(alpha=0.6,col="blue") + coord_flip()
  
   bccvl.savePdf(pprf, ncol=1, nrow=1, filename=filename, aspdf=pdf)                      
 }  
 

 if (!is.na(match("maxent",method))) 
 {
   # variable importance is part of the model fitting outcomes with 'MAXENT.Phillips' algorithm
   if (regexpr("_outputs", filekeep) < 0)
   {
     working <- paste(filekeep,"_outputs/maxentResults.csv",sep="")
   }
   else
   {
     working <- paste(filekeep,"/maxentResults.csv",sep="")
   }

   df.P = read.csv(working)

   the.data <- data1[,-1]
   the.data <- Filter(is.numeric, the.data)
   nx = dim(the.data)[2]    #decide the number of the predictor variables
   yp = as.numeric(df.P[,8:(7+nx)])

   dfp = as.data.frame(cbind(1:nx,yp))
   dfp$V1 = factor(dfp$V1, labels = rev(names(the.data)))
   p <- ggplot(dfp, aes(x=V1, y=rev(yp))) + labs(x="predictor variables") + 
      labs(y="variable relative contribution (%)") + labs(title="maxent algorithm")
   pp = p + geom_col(alpha=0.6,col="red") + coord_flip()

   # Save as variable relative contribution plot.
   filename1 = sub("vip_plot", "variable_relative_contribution", filename)
   bccvl.savePdf(pp, ncol=1, nrow=1, filename=filename1, aspdf=pdf)

   #  the heatmap in terms of correlation among predictor variables
   if(cor.method=="pearson")  xx = cor(the.data)
   if(cor.method=="spearman") xx = cor(the.data,method="spearman")
 
    get_lower_tri<-function(cormat)
    {
      cormat[upper.tri(cormat)] <- NA
      return(cormat) 
    }

   lower_tri = get_lower_tri(xx)
  
   xx.ml <- melt(lower_tri,na.rm=TRUE)  #the argument 'na.rm=TRUE' seems not working)

   corx = xx.ml[,3]
   rm = which(is.na(corx)==TRUE)
   xx.ml = xx.ml[-rm,]

   pheat <- ggplot(xx.ml, aes(X1, X2)) + geom_tile(aes(fill = value), colour="black") + 
      scale_fill_gradient2(low = "green4", high = "violetred", mid="white", midpoint=0, limit=c(-1,1)) +
      scale_x_discrete(limits=rownames(xx)) + scale_y_discrete(limits=colnames(xx)) + coord_fixed() +
      labs(y=" ") + theme_minimal() +
      theme(axis.title.x=element_blank(),legend.position = "bottom", axis.text.x=element_text(angle=-90)) +
      guides(fill=guide_legend(title="correlation"))
   # Save as variable correlation plot.
   filename1 = sub("vip_plot", "variable_correlations", filename)
   bccvl.savePdf(pheat, ncol=1, nrow=1, filename=filename1, aspdf=pdf)
 }
}


########################################################
# Patch Biomod2 plot projection function
#  taken from: biomod2-3.1-64 : https://github.com/cran/biomod2/blob/3.1-64/R/BiomodClass.R#L1586
########################################################
require('rasterVis')
setMethod('plot', signature(x='BIOMOD.projection.out', y="missing"),
          function(x,col=NULL, str.grep=NULL, on_0_1000=TRUE){
            models_selected <- x@models.projected 
            if(length(str.grep)){
              models_selected <- grep(paste(str.grep,collapse="|"), models_selected,value=T)
            } 
            
            if(!length(models_selected)) stop("invalid str.grep arg")

            if(class(x@proj) == "BIOMOD.stored.raster.stack"){
              require(rasterVis)

              my.scale = ifelse(on_0_1000, 1, 1000)
              ## define the breaks of the color key
              my.at <- seq(0,1000,by=100) / my.scale
              ## the labels will be placed vertically centered
              my.labs.at <- seq(0,1000,by=250) / my.scale
              ## define the labels
              my.lab <- seq(0,1000,by=250) / my.scale
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
