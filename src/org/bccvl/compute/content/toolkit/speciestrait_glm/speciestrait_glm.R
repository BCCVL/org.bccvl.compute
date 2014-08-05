
### glm.R ###


## Get the data

glm.data = read.table(bccvl.params$data_table$filename, header=T, sep=",")

## Set up the function call expression
glm.params = list(data=glm.data)

## set defaults for missing parameters according to R docs:
## see Jon Shuker's inputs specification for reference
glm.defaults = list(family=gaussian(link=identity),
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
                   include_intercept=TRUE,
                   contrasts=NULL)

# plain old parameters
for (paramname in c('formula', 'family', 'na.action', 'method', 'model', 'x', 'y', 'include_intercept')) {
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



## Run the regression
glm.result = glm(formula=glm.params$formula,
                 data=glm.params$data,
                 family=glm.params$family,
                 subset=glm.params$subset,
                 weights=glm.params$weights,
                 na.action=glm.params$na.action[[1]],
                 start=glm.params$start,
                 etastart=glm.params$eta_start,
                 mustart=glm.params$mu_start,
                 offset=glm.params$offset,
                 method=glm.params$method,
                 model=glm.params$model,
                 x=glm.params$x,
                 y=glm.params$y,
                 include_intercept=glm.params$include_intercept,
                 contrasts=glm.params$contrasts)

## Save the result to file

bccvl.save(glm.result, "glm.model.object.RData")

## Save result summary to a text file

sink(file="glm_result_summary.txt")
summary(glm.result)
sink()
