#########################################
###        speciestrait_cta.R         ###
#########################################

### Runs a Classification Tree Analysis (for categorical trait data) or a Regression Tree Analysis (for continuous trait data) 
### to test the effect of selected environmental variables on species traits

## trait dataset csv file
trait.data.filename = bccvl.params$traits_dataset$filename
# mapping of variable names of trait dataset
trait.data.varnames = bccvl.params$traits_dataset_params

# read in the trait data
trait.data = read.csv(trait.data.filename)
# Loop through the trait data variables name to extract trait and env data


env.data <- bccvl.params$traits_dataset_params$EnvVar1 # CH: same question, how do we make sure we select all env variables selected here?

## Set parameters (need to be adjusted to link to back end bccvl.params file)

# CH: do we preset the formula in the params file, or do we need to write a loop here so the model will be run for each trait?
# CH: as in comments below the method is the only configuration option that is different for categorical vs continuous traits - this
# should be fixed by us and not changeable by user

trait.cta.options <- list(formula = bccvl.params$formula, # formula should be: trait ~ env1 + env2 + env3 etc 
                          method = bccvl.params$method, # should be "class" for categorical trait data, and "anova" for continuous trait data
                          na.action = na.rpart, # default action deletes observations for which trait value is missing, but keeps those in which one or more environmental variables are missing
                          model = FALSE,
                          x = FALSE,
                          y = FALSE,
                          control = list(minsplit = bccvl.params$control_minsplit, #the minimum number of observations that must exist in a node in order for a split to be attempted
                                         minbucket = bccvl.params$control_minbucket, #the minimum number of observations in any terminal node
                                         cp = bccvl.params$control_cp, #complexity parameter
                                         maxcompete = bccvl.params$control_maxcompete, # number of competitor splits retained in the output
                                         maxsurrogate = bccvl.params$control_maxsurrogate, # number of surrogate splits retained in the output
                                         usesurrogate = bccvl.params$control_usesurrogate, # how to use surrogates in splitting process
                                         surrogatestyle = bccvl.params$control_surstyle, # controls the selection of a best surrogate
                                         xval = bccvl.params$control_xval, #number of cross-validations
                                         maxdepth = bccvl.params$control_maxdepth  #Set the maximum depth of any node of the final tree, with the root node counted as depth 0. Values greater than 30 rpart will give nonsense results on 32-bit machines
                                         )
                          )

# Default values for control parameters:
# minsplit = 20
# minbucket = round(minsplit/3)
# cp = 0.01
# maxcompete = 4
# maxsurrogate = 5
# usesurrogate = 2
# surrogatestyle = 0
# xval = 10
# maxdepth = 30

## Run rpart model

trait.cta = rpart(formula = trait.cta.options$formula,
                  data = ,#?? CH: how to link to data?
                  method = trait.cta.options$method,
                  na.action = trait.cta.options$na.action,
                  model = trait.cta.options$model,
                  x = trait.cta.options$x,
                  y = trait.cta.options$y,
                  control = trait.cta.options$control)

### Save the results to file

summary(trait.cta) # CH: this should be saved as a result and displayed as txt file
printcp(trait.cta) # CH: this should be saved as a result and displayed as table (or txt file if easier for now)
plot(trait.cta) # CH: this should be saved as a result and displayed as figure

bccvl.save(trait.cta, "trait.cta.object.RData")



