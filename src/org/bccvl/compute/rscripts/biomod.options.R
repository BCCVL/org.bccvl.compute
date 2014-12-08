

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

        biomod.options.list = list(quant = bccvl.params$Quant)
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


