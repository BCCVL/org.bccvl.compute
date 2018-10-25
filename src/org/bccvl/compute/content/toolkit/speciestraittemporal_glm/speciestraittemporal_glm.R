
#########################################
###    speciestraittemporal_glm.R     ###
#########################################

### Runs a Generalized Linear Model to test the effect of selected environmental variables on species traits

## DATA

# Link to input dataset csv file merged with the env data
trait.data.filename = bccvl.params$traits_dataset$filename
# Link to variable names of input dataset
trait.data.params = bccvl.params$traits_dataset_params
# Read in the trait data
trait.data = read.csv(trait.data.filename)
# Get the species
trait.species =bccvl.params$species

# Filter for species specified
if (!is.null(trait.data)) {
    trait.data = subset(trait.data, species==trait.species)
    # save it as trait-env data
    traitenv_filename = sprintf("%s_trait_environmental.csv", trait.species)
    bccvl.write.csv(trait.data, traitenv_filename)
}

## MODEL
  
# Load the library
library("MASS")
library("nnet")  

# Generate a formula for each trait
formulae = bccvl.trait.gen_formulae(trait.data.params)
for (formula in formulae) {

# Run model - with polr function for ordinal traits, multinom function for nominal traits, glm function for continuous traits
  na_action = get0(bccvl.params$na_action)
  if (is.null(na_action)) {
        na_action = get("na.fail")
  }
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
