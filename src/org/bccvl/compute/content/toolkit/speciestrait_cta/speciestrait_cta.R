#########################################
###        speciestrait_cta.R         ###
#########################################

### Runs a Classification Tree Analysis (for categorical trait data) or a Regression Tree Analysis (for continuous trait data) 
### to test the effect of selected environmental variables on species traits

## trait dataset csv file
trait.data.filename = bccvl.params$traits_dataset$filename
# mapping of variable names of trait dataset
trait.data.params = bccvl.params$traits_dataset_params

# read in the trait data as data frame
trait.data = read.csv(trait.data.filename)

#define the current enviro data to use
enviro.data.current = lapply(bccvl.params$environmental_datasets, function(x) x$filename)
#type in terms of continuous or categorical
enviro.data.type = lapply(bccvl.params$environmental_datasets, function(x) x$type)
#layer names for the current environmental layers used
enviro.data.layer = lapply(bccvl.params$environmental_datasets, function(x) x$layer)
#geographic constraints
enviro.data.constraints = bccvl.params$modelling_region
# resampling (up / down scaling) if scale_down is TRUE, return 'lowest'
enviro.data.resampling = ifelse(is.null(bccvl.params$scale_down) ||
                                as.logical(bccvl.params$scale_down),
                                'highest', 'lowest')

# read current climate data
current.climate.scenario = bccvl.enviro.stack(enviro.data.current, enviro.data.type, enviro.data.layer, resamplingflag=enviro.data.resampling)

# geographically constrained modelling and merge the env data into trait.data
if (!is.null(trait.data)) {
  trait.data = bccvl.trait.constraint.merge(current.climate.scenario, trait.data, enviro.data.constraints);

  # Update the dataset params with the merged env variables types
  for (i in 1:length(enviro.data.layer)) {
    colname <- enviro.data.layer[[i]]
    trait.data.params[colname] = ifelse(enviro.data.type[i] == 'continuous', 'env_var_con', 'env_var_cat')
  }
}

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
        } else if (colval == 'trait_cat') {
            cols[['trait']][colname] = 'categorical'
        } else if (colval == 'trait_con') {
            cols[['trait']][colname] = 'continuous'
        }
    }
    formulae = list()
    envvars = paste(names(cols[['env']]), collapse=' + ')
    for (trait in names(cols[['trait']])) {
        formulae = append(formulae, list(list(formula=paste(trait, '~', envvars),
                                         method=ifelse(cols[['trait']][trait] == 'categorical', 'class', 'anova'), trait=trait)))
    }
    # return a list of lists, where each sublist has $formula, $method elements, and $trait
    return (formulae)
}

# Genrate formula for each trait, and run each formula
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