#########################################
###        exploration_plot.R         ###
#########################################

### Exploration plots for sepcies trait experiment

## DATA

# Link to input dataset csv file
trait.data.filename = bccvl.params$traits_dataset$filename
# Link to variable names of input dataset
trait.data.params = bccvl.params$traits_dataset_params
# Read in the trait data
trait.data = read.csv(trait.data.filename, header=TRUE, stringsAsFactors=FALSE)
# Get the species
trait.species =bccvl.params$species

# Define the current environmental data to use
enviro.data.current = lapply(bccvl.params$environmental_datasets, function(x) x$filename)
# Type in terms of continuous or categorical
enviro.data.type = lapply(bccvl.params$environmental_datasets, function(x) x$type)
# Layer names for the current environmental layers used
enviro.data.layer = lapply(bccvl.params$environmental_datasets, function(x) x$layer)
#geographic constraints.
enviro.data.constraints = readLines(bccvl.params$modelling_region$filename)
#Indicate to generate and apply convex-hull polygon of occurrence dataset to constraint
enviro.data.generateCHall = ifelse(is.null(bccvl.params$generate_convexhull), FALSE, as.logical(bccvl.params$generate_convexhull))

# Load the environmental raster layers
environ.rasterstack = bccvl.enviro.stack(enviro.data.current, enviro.data.type, enviro.data.layer, "highest")

# if no species, then run across all species
if (!is.null(trait.species)) {
  trait.data = subset(trait.data, species==trait.species)
} else {
  # use the trait filename as species name for the plots' filenames.
  trait.species = file_path_sans_ext(basename(trait.data.filename))
}

# Geographically constrained modelling and merge the environmental data into trait.data
if (!is.null(trait.data) || enviro.data.generateCHall) {
    merged.result = bccvl.trait.constraint.merge(trait.data, trait.data.params, environ.rasterstack, enviro.data.constraints, enviro.data.generateCHall, generateGeoconstraint=FALSE)
    trait.data = merged.result$data
    trait.data.params = merged.result$params
}


# Continuous traits
cont_traits = NULL
cat_traits = NULL
cont_env = NULL
cat_env = NULL 


for (colname in names(trait.data.params)) {
    colval = trait.data.params[[colname]]

    if (colval == 'trait_con') {
    	cont_traits = c(cont_traits, colname)
    }
    else if (colval == 'trait_ord' || colval == 'trait_nom') {
        cat_traits = c(cat_traits, colname)
    }
    else if (colval == 'env_var_con') {
    	cont_env = c(cont_env, colname)
    }
    else if (colval == 'env_var_cat') {
    	cat_env = c(cat_env, colname)
    }
}

# Continuous traits
if (!is.null(cont_traits)) {
	bccvl.exploratoryPlots.contin(trait.data[, cont_traits, drop=FALSE], 
                      doCorrelation = FALSE, 
                      outerTitle = paste0(trait.species, ': Continuous traits'),
                      fnamePrefix = paste0(trait.species, '_continuous_traits_'))
}

# Categorical traits
if (!is.null(cat_traits)) {
	bccvl.exploratoryPlots.categ(trait.data[, cat_traits, drop=FALSE], 
                     outerTitle = paste0(trait.species, ': Categorical traits'),
                     fnamePrefix = paste0(trait.species, '_categorical_traits_'))
}

# Continuous predictor variables
if (!is.null(cont_env)) {
	bccvl.exploratoryPlots.contin(trait.data[, cont_env, drop=FALSE],
                      doCorrelation = TRUE, 
                      outerTitle = paste0(trait.species, ': Continuous predictor variables'), 
                      fnamePrefix = paste0(trait.species, '_continuous_predictors_'))
}

# Categorical predictor variables
if (!is.null(cat_env)) {
	bccvl.exploratoryPlots.categ(trait.data[, cat_env, drop=FALSE], 
                     outerTitle = paste0(trait.species, ': Categorical predictor variables'),
                     fnamePrefix = paste0(trait.species, '_categorical_predictors_'))
}
