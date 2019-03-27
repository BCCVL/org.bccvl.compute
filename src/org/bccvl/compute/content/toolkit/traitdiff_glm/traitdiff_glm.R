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
#geographic constraints.
enviro.data.constraints = readLines(bccvl.params$modelling_region$filename)

# Empty environmental dataset as it is not needed
environ.rasterstack = stack()
crs(environ.rasterstack) <- '+init=epsg:4326'

# Geographically constrained modelling; just need to constraint the trait-data
if (!is.null(trait.data)) {
    merged.result = bccvl.trait.constraint.merge(trait.data, trait.data.params, environ.rasterstack, enviro.data.constraints, generateCHull=FALSE, generateGeoconstraint=FALSE)
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
na_action = get0(bccvl.params$na_action)
if (is.null(na_action)) {
    na_action = get("na.fail")
}
for (formula in formulae) {
    traitname = gsub("[_ ]", "-", trimws(formula$trait))
    if (formula$type == 'ordinal') {
        output_filename = paste0(traitname, "_diffpolr_results.txt")
        glm.result = polr(formula=formula(formula$formula),
                          data=trait.data,
                          weights=NULL,
                          na.action=na_action,
                          contrasts=NULL,
                          Hess=TRUE,
                          model=TRUE,
                          method="logistic")
    } else if (formula$type == 'nominal') {
        output_filename = paste0(traitname, "_diffnom_results.txt")
        glm.result = multinom(formula=formula(formula$formula),
                              data=trait.data,
                              weights=NULL,
                              na.action=na_action,
                              contrasts=NULL,
                              summ=0,        
                              model=TRUE)
    } else {
        output_filename = paste0(traitname, "_diffglm_results.txt")
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
    bccvl.save(glm.result, paste0(traitname, "_diffglm_model.object.RData"))

    ## Save the results as text to file for each trait
    s <- summary(glm.result) 
    bccvl.write.text(s, output_filename)
}
