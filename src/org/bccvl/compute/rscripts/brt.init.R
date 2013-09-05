# This is a Python string template file and is currently tightly tied
# to ../brt.py.
.libPaths("{rlibdir}")
wd = "{workdir}"
species = "{species}"
occur.data = "{occurence}"
bkgd.data = "{background}"
enviro.data.names = c({enviro[names]})
enviro.data.current = c({enviro[data]})
enviro.data.type = c({enviro[type]})
enviro.data.future= c({future[data]})

model.bioclim = FALSE
project.bioclim = FALSE
model.brt = TRUE #boolean to run Boosted regression tree algorithm
project.brt = TRUE #boolean to project Boosted regression tree algorithm

brt.fold.vector = NULL #a fold vector to be read in for cross validation with offsets
brt.tree.complexity = 1 #sets the complexity of individual trees
brt.learning.rate = 0.01 #sets the weight applied to individual trees
brt.bag.fraction = 0.75 #sets the proportion of observations used in selecting variables
#brt.site.weights = rep(1, nrow(data)) #allows varying weighting for sites
#brt.var.monotone = rep(0, length(gbm.x)) #restricts responses to individual predictors to monotone
brt.n.folds = 10 #number of folds
brt.prev.stratify = TRUE #prevalence stratify the folds - only for presence/absence data
brt.family = "bernoulli" #family - bernoulli (=binomial), poisson, laplace or gaussian
brt.n.trees = 50 #number of initial trees to fit
brt.step.size = brt.n.trees #numbers of trees to add at each cycle
brt.max.trees = 10000 #max number of trees to fit before stopping
brt.tolerance.method = "auto" #method to use in deciding to stop - "fixed" or "auto"
brt.tolerance = 0.001 #tolerance value to use - if method == fixed is absolute, if auto is multiplier * total mean deviance
brt.keep.data = FALSE #Logical. keep raw data in final model
brt.plot.main = FALSE #Logical. plot hold-out deviance curve
brt.plot.folds = FALSE #Logical. plot the individual folds as well
brt.verbose = FALSE #Logical. control amount of screen reporting
brt.silent = FALSE #Logical. to allow running with no output for simplifying model)
brt.keep.fold.models = FALSE #Logical. keep the fold models from cross valiation
brt.keep.fold.vector = FALSE #Logical. allows the vector defining fold membership to be kept
brt.keep.fold.fit = FALSE #Logical. allows the predicted values for observations from cross-validation to be kept
