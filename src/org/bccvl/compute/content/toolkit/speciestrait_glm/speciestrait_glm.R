
#########################################
###        speciestrait_glm.R         ###
#########################################

### Runs a Generalized Linear Model to test the effect of selected environmental variables on species traits

## trait dataset csv file
trait.data.filename = bccvl.params$traits_dataset$filename
# mapping of variable names of trait dataset
trait.data.varnames = bccvl.params$traits_dataset_params

# read in the trait data
trait.data = read.csv(trait.data.filename)
# Loop through the trait data variables name to extract trait and env data
for (varname in ls(trait.data.varnames)) {
    if (varname %in% colnames(trait.data)) {
      assign(paste(varname), trait.data[,varname])
    }
}

env.data <- bccvl.params$traits_dataset_params$EnvVar1 # CH: same question, how do we make sure we select all env variables selected here?

## CH: below is old script

## Set up the function call expression # CH: Yong, Gerhard is this still correct?
glm.params = list(data=glm.data)

## set defaults for missing parameters according to R docs:
## see Jon Shuker's inputs specification for reference
glm.defaults = list(family="gaussian(link=identity)",
                   subset=NULL,
                   weights=NULL,
                   na.action=options("na.action")[[1]],
                   start=NULL,
                   ea_start=NULL,
                   mu_start=NULL,
                   offset=NULL,
                   method="glm.fit",
                   model=TRUE,
                   x=FALSE,
                   y=FALSE,
                   contrasts=NULL)

# plain old parameters
for (paramname in c('formula', 'family', 'na.action', 'method', 'model', 'x', 'y')) {
    if (! is.null(bccvl.params[[paramname]])) {
        glm.params[paramname] = bccvl.params[paramname]
    } else {
        glm.params[paramname] = glm.defaults[paramname]
    }
}

# parameters that sholud refer to a column in glm.data
for (paramname in c('start', 'eta_start', 'mu_start', 'subset', 'weights', 'contrasts','offset')) {
    if (! is.null(bccvl.params[[paramname]])) {
        glm.params[paramname] = glm.data[bccvl.params[[paramname]]]
    } else {
        glm.params[paramname] = glm.defaults[paramname]
    }
}

# singular.ok has a different name in bccvl.params
if (! is.null(bccvl.params['singular_ok'])) {
    glm.params['singular.ok'] = bccvl.params$singular_ok
} else {
    glm.params['singular.ok'] = glm.defaults$singular.ok
}

# parse the family string (character) into a proper family object
glm.params$family=family_from_string(glm.params$family)


## Run the regression
glm.result = glm(formula=glm.params$formula, ## formula should be: trait ~ env1 + env2 + env3 etc 
                 family=glm.params$family,
                 data=glm.params$data,
                 weights=glm.params$weights,
                 subset=glm.params$subset,
                 na.action=glm.params$na.action[[1]],
                 start=glm.params$start,
                 etastart=glm.params$eta_start,
                 mustart=glm.params$mu_start,
                 offset=glm.params$offset,
                 model=glm.params$model,
                 method=glm.params$method,
                 x=glm.params$x,
                 y=glm.params$y,
                 contrasts=glm.params$contrasts)

## Save the result to file

bccvl.save(glm.result, "glm.model.object.RData")

## Save result summary to a text file

sink(file="glm_result_summary.txt")
summary(glm.result)
sink()
