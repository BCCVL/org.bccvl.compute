
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

# if no species, the run across all species
if (!is.null(trait.species)) {
  trait.data = subset(trait.data, species==trait.species)
} else {
  # use the trait filename as species name for the plots' filenames.
  trait.species = file_path_sans_ext(basename(trait.data.filename))
}

projection.name = "current"
species_algo_str = sprintf("%s_glm", trait.species)

# Geographically constrained modelling and merge the environmental data into trait.data
if (!is.null(trait.data)) {
    merged.result = bccvl.trait.constraint.merge(trait.data, trait.data.params, environ.rasterstack, enviro.data.constraints, enviro.data.generateCHall)
    trait.data = merged.result$data
    trait.data.params = merged.result$params
    environ.constrained.rasterstack = merged.result$raster
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
  # Replace underscore and white-space with dash in trait-name for use in output filename
  traitname = gsub("[_ ]", "-", trimws(formula$trait))
  if (formula$type == 'ordinal') {
        library(visreg)
        output_filename = paste0(traitname, "_polr_results.txt")
        glm.result = polr(formula=formula(formula$formula),
                          data=trait.data,
                          weights=NULL,
                          na.action=na_action,
                          contrasts=NULL,
                          Hess=TRUE,
                          model=TRUE,
                          method="logistic")
        visreg(glm.result)
    } else if (formula$type == 'nominal') {
        output_filename = paste0(traitname, "_nom_results.txt")
        glm.result = multinom(formula=formula(formula$formula),
                              data=trait.data,
                              weights=NULL,
                              na.action=na_action,
                              contrasts=NULL,
                              summ=0,
                              model=TRUE)
    } else {
        output_filename = paste0(traitname, "_glm_results.txt")
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
        # regression and iagnostic plots
        bccvl.regPlots(glm.result, 
                       outerTitle = paste0(trait.species, ': GLM fit for ', formula$trait), 
                       fnamePrefix = paste0(traitname, '_', trait.species, '_glm_'))

        # Do projection only if there is no fixed factors i.e. all env variables of env dataset.
        env.names = names(environ.rasterstack)
        if (all(unlist(lapply(formula$env, function(x) x %in% env.names)))) {
          # Do projection over constrained region
          if (is.null(environ.constrained.rasterstack)) {
            env.data = as.data.frame(environ.rasterstack, xy=TRUE)
          }
          else {
            env.data = as.data.frame(environ.constrained.rasterstack, xy=TRUE)
          }
          proj  = predict.glm(glm.result, env.data)

          # combine the coordinates and projection value to generate a raster
          proj1 = cbind(env.data[,c("x", "y")], proj)
          proj.raster = rasterFromXYZ(proj1)

          # Save projection as geotif and png
          bccvl.saveModelProjection(proj.raster, projection.name, formula$trait, species_algo_str)

          # Do projection over unconstrained region only if all env variables are not categorical
          if (!is.null(environ.constrained.rasterstack) && all(unlist(lapply(environ.rasterstack@layers, function(lyr) return(!lyr@data@isfactor))))) {
            env.data = as.data.frame(environ.rasterstack, xy=TRUE)
            proj  = predict.glm(glm.result, env.data)
          
            # combine the coordinates and projection value to generate a raster
            proj1 = cbind(env.data[,c("x", "y")], proj)
            proj.raster = rasterFromXYZ(proj1)
          
            # Save projection as geotif and png
            bccvl.saveModelProjection(proj.raster, projection.name, formula$trait, species_algo_str, filename_ext="unconstrained")
          }
        }
    }

  # Save the model to file
  bccvl.save(glm.result, paste0(traitname, "_glm_model.object.RData"))

  ## Save the results as text to file for each trait
  s <- summary(glm.result) 
  bccvl.write.text(s, output_filename)
}
