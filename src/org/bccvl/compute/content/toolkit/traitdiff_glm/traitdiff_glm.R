#########################################
###        traitdiff_glm.R         ###
#########################################

### Runs a Generalized Linear Model to test how traits differ among species

## DATA

# Link to input dataset csv file
trait.data.filename = bccvl.params$traits_dataset$filename
# Link to variable names of input dataset
trait.data.varnames = bccvl.params$traits_dataset_params
# Read in the trait data
trait.data = read.csv(trait.data.filename)

# Loop through the trait data variables name to extract trait and environmental data
for (varname in ls(trait.data.varnames)) {
    if (varname %in% colnames(trait.data)) {
      assign(paste(varname), trait.data[,varname])
    }
}

## MODEL

# Load the library
library("MASS")
library("nnet")  

# Generate the formulae to test differences among species for each trait separately
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
        formulae = append(formulae, list(list(formula=paste(trait, '~', species)
    }
    # return a list of lists, where each sublist has $formula
    return (formulae)
}

# Run model - with polr function for ordinal traits, multinom function for nominal traits, glm function for continuous traits
formulae = bccvl.trait.gen_formulae(trait.data.params)
for (formula in formulae) {
  if (formula$type == 'ordinal') {
        output_filename = paste0(formula$trait, ".polr.results.txt")
        glm.result = polr(formula=formula(formula$formula),
                          data=trait.data,
                          weights=NULL,
                          na.action=bccvl.params$na_action,
                          contrasts=NULL,
                          model=TRUE,
                          method="logistic")
    } else if (formula$type == 'nominal') {
        output_filename = paste0(formula$trait, ".nom.results.txt")
        glm.result = multinom(formula=formula(formula$formula),
                              data=trait.data,
                              weights=NULL,
                              na.action=bccvl.params$na_action,
                              contrasts=NULL,
                              summ=0,        
                              model=TRUE)
    } else {
        output_filename = paste0(formula$trait, ".glm.results.txt")
        glm.result = glm(formula=formula(formula$formula),
                         family=bccvl.params$family,
                         data= trait.data
                         weights=NULL,
                         na.action=bccvl.params$na_action,
                         start=NULL,
                         etastart=NULL,
                         mustart=NULL,
                         offset=NULL,
                         model=TRUE,
                         method=bccvl.params$method,
                         x=FALSE,
                         y=FALSE,
                         contrasts=NULL)
}
    
    
## Save the result to file
# Save the model
bccvl.save(glm.result, paste0(formula$trait, ".glm.model.object.RData")

## Save the results as text to file for each trait
s <- summary(glm.result) 
bccvl.write.text(s, output_filename)                                       
}
    
    
