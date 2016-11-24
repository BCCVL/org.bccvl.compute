#########################################
###        traitdiff_glm.R         ###
#########################################

### Runs a Generalized Linear Model to test how traits differ among species

## DATA

# Link to input dataset csv file
trait.data.filename = bccvl.params$traits_dataset$filename
# Link to variable names of input dataset
trait.data.params = bccvl.params$traits_dataset_params
# Read in the trait data
trait.data = read.csv(trait.data.filename)
# Geographic constraints
enviro.data.constraints = bccvl.params$modelling_region

# Empty environmental dataset as it is not needed
enviro.data.current = list()

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

# Generate the formulae to test differences among species for each trait separately
formulae = bccvl.trait.gen_formulae(trait.data.params, trait_diff=TRUE)
# Run model - with polr function for ordinal traits, multinom function for nominal traits, glm function for continuous traits
na_action = get(getOption(bccvl.params$na_action, "na.fail"))
for (formula in formulae) {
    if (formula$type == 'ordinal') {
        output_filename = paste0(formula$trait, ".diffpolr.results.txt")
        glm.result = polr(formula=formula(formula$formula),
                          data=trait.data,
                          weights=NULL,
                          na.action=na_action,
                          contrasts=NULL,
                          Hess=TRUE,
                          model=TRUE,
                          method="logistic")
    } else if (formula$type == 'nominal') {
        output_filename = paste0(formula$trait, ".diffnom.results.txt")
        glm.result = multinom(formula=formula(formula$formula),
                              data=trait.data,
                              weights=NULL,
                              na.action=na_action,
                              contrasts=NULL,
                              summ=0,        
                              model=TRUE)
    } else {
        output_filename = paste0(formula$trait, ".diffglm.results.txt")
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
    bccvl.save(glm.result, paste0(formula$trait, ".diffglm.model.object.RData"))

    ## Save the results as text to file for each trait
    s <- summary(glm.result) 
    bccvl.write.text(s, output_filename)
}
