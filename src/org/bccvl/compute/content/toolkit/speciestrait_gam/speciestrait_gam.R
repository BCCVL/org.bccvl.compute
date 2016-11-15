
#########################################
###        speciestrait_gam.R         ###
#########################################

### Runs a Generalized Additive Model to test the effect of selected environmental variables on species traits

## trait dataset csv file
trait.data.filename = bccvl.params$traits_dataset$filename
# mapping of variable names of trait dataset
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

# load the gam Library
library("gam")

# Read current climate data
current.climate.scenario = bccvl.enviro.stack(enviro.data.current, enviro.data.type, enviro.data.layer, resamplingflag=enviro.data.resampling)

# Geographically constrained modelling and merge the env data into trait.data
if (!is.null(trait.data)) {
    trait.data = bccvl.trait.constraint.merge(current.climate.scenario, trait.data, enviro.data.constraints);

    # Update the dataset params with the merged env variables types
    if (length(current.climate.scenario)) {
        for (i in 1:length(enviro.data.layer)) {
          colname <- enviro.data.layer[[i]]
          trait.data.params[colname] = ifelse(enviro.data.type[[i]] == 'continuous', 'env_var_con', 'env_var_cat')
        }
    }
}


## Set up the function call expression
# Generate a formula for each trait
formulae = bccvl.trait.gen_formulae(trait.data.params)
for (formula in formulae) {
    trait_name = formula$trait

    ## Run the model
    gam.result = gam(formula=formula,
                     data=trait.data,
                     family=family_from_string(bccvl.params$family)
                     subset=bccvl.params$subset,
                     weights=bccvl.params$weights,
                     na.action=bccvl.params$na_action,
                     start=bccvl.params$start,
                     etastart=bccvl.params$eta_start,
                     mustart=bccvl.params$mu_start,
                     method=bccvl.params$method,
                     model=bccvl.params$model,
                     x=bccvl.params$x,
                     y=bccvl.params$y,
                     contrasts=NULL)

    ## Save the result to file
    # Save the model
    bccvl.save(gam.result, paste0(trait_name, ".gam.model.object.RData"))

    ## Save result summary to a text file
    s <- summary(gam.result)
    bccvl.write.text(s, paste0(trait_name, ".gam_result_summary.txt"))

    # save the plot as png image
    ofilename = paste0(trait_name, ".gam.plotgam")
    bccvl.write.image(trait.cta, ofilename, "plot.gam")
}