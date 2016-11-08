
#########################################
###        traitdiff_glm.R         ###
#########################################

### Runs a Generalized Linear Model to test how traits differ among species

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

# Generate formula to test differences in traits between species
## CH: we don't need environmental variables in this formula, so maybe we can delete lines 37-40 ?

gen_formulae <- function(dataset_params) {
    cols = list(species=list(),
                lat=list(),
                lon=list(),
                env=list(),
                trait=list())
    for(colname in names(dataset_params)) {
        colval = dataset_params[[colname]]
        if (colval == 'species' || colval == 'lon' || colval == 'lat') {
            cols[[colval]][colname] = colval
        } else if (colval == 'env_var_cat') {
            cols[['env']][colname] = 'categorical'
        } else if (colval == 'env_var_con') {
            cols[['env']][colname] = 'continuous'
        } else if (colval == 'trait_cat') {
            cols[['trait']][colname] = 'categorical'
        } else if (colval == 'trait_con') {
            cols[['trait']][colname] = 'continuous'
        }
    }
    formulae = list()
    envvars = paste(names(cols[['env']]), collapse=' + ') # CH: do we need this line if no env variables are used in this?
    for (trait in names(cols[['trait']])) {
        formulae = append(formulae, list(list(formula=paste(trait, '~', species)
    }
    # return a list of lists, where each sublist has $formula #CH: method not used here so deleted.
    return (formulae)
}

## Set up the function call expression
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

# CH: do we need lines below (76-99) if params are defined in params.json file??
# plain old parameters
for (paramname in c('formula', 'family', 'na.action', 'method', 'model', 'x', 'y')) {
    if (! is.null(bccvl.params[[paramname]])) {
        glm.params[paramname] = bccvl.params[paramname]
    } else {
        glm.params[paramname] = glm.defaults[paramname]
    }
}

# parameters that sholud refer to a column in glm.data # CH: these are now not visible to the user any more, thus not changeable, so can
# we get rid of lines below?
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
glm.result = glm(formula=glm.params$formula,
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
