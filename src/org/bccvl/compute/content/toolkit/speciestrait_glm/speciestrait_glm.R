
#########################################
###        speciestrait_glm.R         ###
#########################################

### Runs a Generalized Linear Model to test the effect of selected environmental variables on species traits

## DATA

# Link to input dataset csv file
trait.data.filename = bccvl.params$traits_dataset$filename
# Link to variable names of input dataset
trait.data.varnames = bccvl.params$traits_dataset_params

# Read in the trait data
trait.data = read.csv(trait.data.filename)
# Loop through the trait data variables names to extract trait and environmental data
for (varname in ls(trait.data.varnames)) {
    if (varname %in% colnames(trait.data)) {
      assign(paste(varname), trait.data[,varname])
    }
}

# TODO: Read in other env variables - below is copied from SDM

# Define the current environmental data to use
enviro.data.current = lapply(bccvl.params$environmental_datasets, function(x) x$filename)
# Type in terms of continuous or categorical
enviro.data.type = lapply(bccvl.params$environmental_datasets, function(x) x$type)
# Layer names for the current environmental layers used
enviro.data.layer = lapply(bccvl.params$environmental_datasets, function(x) x$layer)
# Geographic constraints
enviro.data.constraints = bccvl.params$modelling_region

## MODEL

# Generate formulae for additive models that test the response of each trait (1 per model) to all environmental variables selected
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
                                          type=cols[['trait']]['trait']))
  }
  # return a list of lists, where each sublist has $formula, $trait and $type
  return (formulae)
}

# Run model

for (formula in formulae) {
    form = formula$formula
        if (formula$type == 'ordinal') 
            {trait.polr = polr(formula=formula(formula$formula),
                  data= # TODO: do we need another link to the data here?
                  weights=NULL,
                  na.action=bccvl.params$na.action,
                  contrasts=NULL,
                  model=, #CH check whether this should be true or false
                  method="logistic")              
        } else if (formula$method == 'nominal') 
            {trait.multinom = multinom(formula=formula(formula$formula),
                  data= # TODO: do we need another link to the data here?
                  weights=NULL,
                  na.action=bccvl.params$na.action,
                  contrasts=NULL,
                  summ=0,        
                  model=,) #CH check whether this should be true or false            
        } else # for continuous traits:
            {trait.glm = glm(formula=formula(formula$formula),
                family=bccvl.params$family,
                data= # TODO: do we need another link to the data here?
                weights=NULL,
                na.action=bccvl.params$na.action,
                start=NULL,
                etastart=NULL,
                mustart=NULL,
                offset=NULL,
                model=, #CH check whether this should be true or false
                method=bccvl.params$method,
                x=FALSE,
                y=FALSE,
                contrasts=NULL)
            } }                             

## Save the results as text to file for each trait
con <- summary(trait.glm) 
ord <- summary(trait.polr)
nom <- summary(trait.multinom)
bccvl.write.text(glm, paste0(trait_name, ".glm.results.txt"))
bccvl.write.text(ord, paste0(trait_name, ".polr.results.txt"))
bccvl.write.text(nom, paste0(trait_name, ".nom.results.txt"))

                                                 
## Save the result to file
bccvl.save(glm.result, "glm.model.object.RData")
