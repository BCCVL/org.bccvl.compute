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
# Loop through the trait data variables names to extract trait and environmental data
for (varname in ls(trait.data.varnames)) {
    if (varname %in% colnames(trait.data)) {
      assign(paste(varname), trait.data[,varname])
    }
}

# TODO: Read in other env variables - CH: lines below copied from SDM, not complete yet

# Define the current environmental data to use
enviro.data.current = lapply(bccvl.params$environmental_datasets, function(x) x$filename)
# Type in terms of continuous or categorical
enviro.data.type = lapply(bccvl.params$environmental_datasets, function(x) x$type)
# Layer names for the current environmental layers used
enviro.data.layer = lapply(bccvl.params$environmental_datasets, function(x) x$layer)
# Geographic constraints
enviro.data.constraints = bccvl.params$modelling_region

## MODEL

# Generate formulae for models that test the response of each trait (1 per model) to all environmental variables selected
# e.g. trait ~ env1 + env2 + env3 etc.
gen_formulae <- function(dataset_params) {
    cols = list(species=list(),
                lat=list(),
                lon=list(),
                env=list(),
                trait=list())
    for(colname in names(dataset_params)) {
    colval = dataset_params[[colname]]
        if (colval == 'species' || colval == 'lon' || colval == 'lat') {
            cols[[colval]][colname] = colval
        } else if (colval == 'env_var_cat') {
            cols[['env']][colname] = 'categorical'
        } else if (colval == 'env_var_con') {
            cols[['env']][colname] = 'continuous'
        } else if (colval == 'trait_ord') {
            cols[['trait']][colname] = 'ordinal'
        } else if (colval == 'trait_nom') {
            cols[['trait']][colname] = 'nominal'
        } else if (colval == 'trait_con') {
            cols[['trait']][colname] = 'continuous'
        }
  }
    formulae = list()
    envvars = paste(names(cols[['env']]), collapse=' + ')
    for (trait in names(cols[['trait']])) {
        formulae = append(formulae, list(list(formula=paste(trait, '~', envvars),
                                              method=ifelse(cols[['trait']][trait] == 'continuous', 'anova', 'class'), trait=trait)))
                                                # CH: Yong, can you check if this way of assignging method 'anova' to continuous traits,
                                                # and method 'class' to the other two (ordinal and nominal) is correct?
    }
    # return a list of lists, where each sublist has $formula, $method, and $trait
    return (formulae)
}

# Run models
    
formulae = gen_formulae(trait.data.params)
for (formula in formulae) {
    trait_name = formula$trait
    trait.cta.options <- list(formula = formula(formula$formula), # formula should be: trait ~ env1 + env2 + env3 etc 
                              method = formula$method, # should be "class" for categorical trait data, and "anova" for continuous trait data
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
ofilename = paste0(trait_name, ".cta.plotcp")
bccvl.write.image(trait.cta, ofilename, "plotcp")

# Save the model
bccvl.save(trait.cta, paste0(trait_name, ".cta.object.RData"))
}
