#########################################
###        speciestrait_cta.R         ###
#########################################

### Runs a Classification Tree Analysis (for categorical trait data) or a Regression Tree Analysis (for continuous trait data) 
### to test the effect of selected environmental variables on species traits

## DATA

# Link to input dataset csv file
trait.data.filename = bccvl.params$traits_dataset$filename
# Link to variable names of input dataset
trait.data.params = bccvl.params$traits_dataset_params
# Read in the trait data
trait.data = read.csv(trait.data.filename)

# Define the current environmental data to use
enviro.data.current = lapply(bccvl.params$environmental_datasets, function(x) x$filename)
# Type in terms of continuous or categorical
enviro.data.type = lapply(bccvl.params$environmental_datasets, function(x) x$type)
# Layer names for the current environmental layers used
enviro.data.layer = lapply(bccvl.params$environmental_datasets, function(x) x$layer)
# Geographic constraints
enviro.data.constraints = bccvl.params$modelling_region

# Load the rpart library
library("rpart")

# Geographically constrained modelling and merge the environmental data into trait.data
if (!is.null(trait.data)) {
    merged.result = bccvl.trait.constraint.merge(trait.data, trait.data.params, enviro.data.current, enviro.data.type, enviro.data.layer, enviro.data.constraints);
    trait.data = merged.result$data
    trait.data.params = merged.result$params
}


# Run models
# Generate a formula for each trait
formulae = bccvl.trait.gen_formulae(trait.data.params)
for (formula in formulae) {
    trait_name <- formula$trait
    trait.cta.options <- list(formula = formula(formula$formula), # formula should be: trait ~ env1 + env2 + env3 etc 
                              method = ifelse(formula$type == 'continuous', 'anova', 'class'), # should be "class" for categorical trait data, and "anova" for continuous trait data
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

    trait.cta = rpart(formula = trait.cta.options$formula,
                      data = trait.data, # data frame containing trait and env data
                      method = trait.cta.options$method,
                      na.action = trait.cta.options$na.action,
                      model = trait.cta.options$model,
                      x = trait.cta.options$x,
                      y = trait.cta.options$y,
                      control = trait.cta.options$control)

    ### Save the results as text to file for each trait
    s <- summary(trait.cta) # saved and displayed as text
    bccvl.write.text(s, paste0(trait_name, ".cta.results.txt"))
    p <- printcp(trait.cta) # saved and displayed as text
    bccvl.write.text(p, paste0(trait_name, ".cta.results.txt"), append=TRUE)

    # save the plot as png image
    png(file.path(bccvl.env$outputdir, paste(trait_name, "cta.plot", "png", sep=".")))
    plot(trait.cta)
    text(trait.cta)
    dev.off()


    # Save the model
    bccvl.save(trait.cta, paste0(trait_name, ".cta.model.object.RData"))
}
