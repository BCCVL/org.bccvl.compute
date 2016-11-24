
#########################################
###        speciestrait_glm.R         ###
#########################################

### Runs a Generalized Linear Model to test the effect of selected environmental variables on species traits

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


# Geographically constrained modelling and merge the environmental data into trait.data
if (!is.null(trait.data)) {
    merged.result = bccvl.trait.constraint.merge(trait.data, trait.data.params, enviro.data.current, enviro.data.type, enviro.data.layer, enviro.data.constraints);
    trait.data = merged.result$data
    trait.data.params = merged.result$params
}

## MODEL
  
# Load the library
library("MASS")
library("nnet")  

# Generate a formula for each trait
formulae = bccvl.trait.gen_formulae(trait.data.params)
for (formula in formulae) {

# Run model - with polr function for ordinal traits, multinom function for nominal traits, glm function for continuous traits
  na_action = get(getOption(bccvl.params$na_action, "na.fail"))
  if (formula$type == 'ordinal') {
        output_filename = paste0(formula$trait, ".polr.results.txt")
        glm.result = polr(formula=formula(formula$formula),
                          data=trait.data,
                          weights=NULL,
                          na.action=na_action,
                          contrasts=NULL,
                          Hess=TRUE,
                          model=TRUE,
                          method="logistic")
    } else if (formula$type == 'nominal') {
        output_filename = paste0(formula$trait, ".nom.results.txt")
        glm.result = multinom(formula=formula(formula$formula),
                              data=trait.data,
                              weights=NULL,
                              na.action=na_action,
                              contrasts=NULL,
                              summ=0,        
                              model=TRUE)
    } else {
        output_filename = paste0(formula$trait, ".glm.results.txt")
        glm.result = glm(formula=formula(formula$formula),
                         family=family_from_string(bccvl.params$family),
                         data= trait.data,
                         weights=NULL,
                         na.action=na_action,
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
bccvl.save(glm.result, paste0(formula$trait, ".glm.model.object.RData"))

## Save the results as text to file for each trait
s <- summary(glm.result) 
bccvl.write.text(s, output_filename)                                       
}
