
#########################################
###        speciestrait_gam.R         ###
#########################################

### Runs a Generalized Additive Model to test the effect of selected environmental variables on species traits

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
# Resampling (up / down scaling) if scale_down is TRUE, return 'lowest'
enviro.data.resampling = ifelse(is.null(bccvl.params$scale_down) ||
                                as.logical(bccvl.params$scale_down),
                                'highest', 'lowest')

# Read current climate data
current.climate.scenario = bccvl.enviro.stack(enviro.data.current, enviro.data.type, enviro.data.layer, resamplingflag=enviro.data.resampling)

# Geographically constrained modelling and merge the environmental data into trait.data
if (!is.null(trait.data)) {
    trait.data = bccvl.trait.constraint.merge(current.climate.scenario, trait.data, enviro.data.constraints);

    # Update the dataset parameters with the merged environmental variables types
    if (length(current.climate.scenario)) {
        for (i in 1:length(enviro.data.layer)) {
          colname <- enviro.data.layer[[i]]
          trait.data.params[colname] = ifelse(enviro.data.type[[i]] == 'continuous', 'env_var_con', 'env_var_cat')
        }
    }
}

  
## MODEL
  
# Load the gam Library
library("gam")

# Generate a formula for each trait
formulae = bccvl.trait.gen_formulae(trait.data.params)
for (formula in formulae) {
    trait_name = formula$trait

    # Run the model for each trait separately
    gam.result = gam(formula=formula(formula$formula),
                     data=trait.data,
                     family=bccvl.params$family,             
                     weights=NULL,
                     na.action=get(getOption(bccvl.params$na_action, "na.fail")),
                     start=NULL,
                     etastart=NULL,
                     mustart=NULL,
                     method=bccvl.params$method,
                     model=TRUE,
                     x=FALSE,
                     y=FALSE)

    ## Save the result to file
    # Save the model
    bccvl.save(gam.result, paste0(trait_name, ".gam.model.object.RData"))

    # Save result summary to a text file
    s <- summary(gam.result)
    bccvl.write.text(s, paste0(trait_name, ".gam_result_summary.txt"))

    # save the plot as png image
    ofilename = paste0(trait_name, ".gam.plotgam")
    bccvl.write.image(gam.result, ofilename, "plot.gam")
}
