
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

# TODO: Read in other env variables

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
  }
  # return a list of lists, where each sublist has $formula and $trait
  return (formulae)
}

# Run model

formulae = gen_formulae(trait.data.params)
for (formula in formulae) {
  trait_name = formula$trait

# TODO: include script/loop to run different function for different categories:

# for continuous traits:
trait.glm = glm(formula=formula(formula$formula),
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

# for ordinal traits:    
trait.polr = polr(formula=formula(formula$formula),
                  data= # TODO: do we need another link to the data here?
                  weights=NULL,
                  na.action=bccvl.params$na.action,
                  contrasts=NULL,
                  model=, #CH check whether this should be true or false
                  method="logistic")   

# for nominal traits:    
trait.multinom = multinom(formula=formula(formula$formula),
                  data= # TODO: do we need another link to the data here?
                  weights=NULL,
                  na.action=bccvl.params$na.action,
                  contrasts=NULL,
                  summ=0,        
                  model=,) #CH check whether this should be true or false
    
                                                  
# TODO:                              
## Save the result to file

bccvl.save(glm.result, "glm.model.object.RData")

## Save result summary to a text file

sink(file="glm_result_summary.txt")
summary(glm.result)
sink()
