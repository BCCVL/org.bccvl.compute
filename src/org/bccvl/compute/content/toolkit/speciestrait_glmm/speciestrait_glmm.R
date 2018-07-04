
#########################################
###        speciestrait_glmm.R         ###
#########################################

### Runs a Generalized Linear Mixed Model to test the effect of selected variables (fixed and random factors) on species traits

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
library(lme4)
library(ordinal)

# Generate a formula for each trait
# trait ~ fixed1 + fixed2 + (1|random1) + (1|random2)
formulae = bccvl.trait.gen_formulae(trait.data.params)
for (formula in formulae) {
  # Run model - with clmm function for ordinal traits, glmer function for nominal traits, glmer function for continuous traits
  # Todo: not sure whether 'glmer' works for nominal trait data - need to further look into this
  na_action = get0(bccvl.params$na_action)
  if (is.null(na_action)) {
      na_action = get("na.fail")
  }

  if (formula$type == 'ordinal') {
        output_filename = paste0(formula$trait, ".clmm.results.txt")
        glmm.result = clmm(formula=formula(formula$formula),
                          data=trait.data,
                          weights=NULL,
                          na.action=na_action,
                          contrasts=NULL,
                          Hess=TRUE,
                          model=TRUE)
    } else if (formula$type == 'nominal') {
        output_filename = paste0(formula$trait, ".nom.results.txt")
        glmm.result = glmer(formula=formula(formula$formula),
                              data=trait.data,
                              weights=NULL,
                              na.action=na_action,
                              contrasts=NULL,
                              summ=0,        
                              model=TRUE)
    } else {
        output_filename = paste0(formula$trait, ".glmer.results.txt")
        glmm.result = glmer(formula=formula(formula$formula),
                         family=family_from_string(bccvl.params$family),
                         data= trait.data,
                         weights=NULL,
                         na.action=na_action,
                         start=NULL,
                         etastart=NULL,
                         mustart=NULL,
                         offset=NULL,
                         contrasts=NULL)
    }

  ## Save the result to file
  # Save the model
  bccvl.save(glmm.result, paste0(formula$trait, ".glmm.model.object.RData"))

  ## Save the results as text to file for each trait
  s <- summary(glmm.result) 
  bccvl.write.text(s, output_filename)                                       
}
