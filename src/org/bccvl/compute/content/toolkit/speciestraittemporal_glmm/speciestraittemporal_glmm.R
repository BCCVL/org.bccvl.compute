
#########################################
###        speciestraittemporal_glmm.R         ###
#########################################

### Runs a Generalized Linear Mixed Model to test the effect of selected variables (fixed and random factors) on species traits

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
library(lme4)
library(ordinal)

# Generate a formula for each trait
# trait ~ fixed1 + fixed2 + (1|random1) + (1|random2)
formulae = bccvl.trait.gen_formulae(trait.data.params, include_rf=TRUE)
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
