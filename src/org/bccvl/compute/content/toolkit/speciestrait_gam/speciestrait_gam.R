
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
# Get the species
trait.species =bccvl.params$species

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
    trait.data = subset(trait.data, species==trait.species)
    merged.result = bccvl.trait.constraint.merge(trait.data, trait.data.params, enviro.data.current, enviro.data.type, enviro.data.layer, enviro.data.constraints);
    trait.data = merged.result$data
    trait.data.params = merged.result$params
}

  
## MODEL
  
# Load the gam Library
library("gam")

# To do: Shall mgcv library instead of gam library?
# It causes issue with gam library
#library("mgcv")

na_action = get0(bccvl.params$na_action)
if (is.null(na_action)) {
    na_action = get("na.fail")
}

# Generate a formula for each trait
formulae = bccvl.trait.gen_formulae(trait.data.params)
for (formula in formulae) {
    trait_name = formula$trait

    if (formula$type == 'ordinal') {
        fam = ocat(R=nlevels(trait.data[[trait_name]]))
    }
    else if (formula$type == 'nominal')
    {
        # env variables for the formula
        envvar = attr(terms(formula(formula$formula)), 'term.labels')
        fam = multinom(K=length(envvar))
    }
    else {
        fam = family_from_string(bccvl.params$family)
    }

    # Run the model for each trait separately
    gam.result = gam(formula=formula(formula$formula),
                     data=trait.data,
                     family=fam,
                     weights=NULL,
                     na.action=na_action,
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
