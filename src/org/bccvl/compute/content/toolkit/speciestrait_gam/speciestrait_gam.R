
### gam.R ###

# TODO: verify with Gerhard that this is the appropriate place to load this
library("gam")

## Get the data

gam.data = read.table(bccvl.params$data_table$filename, header=T, sep=",")

## Set up the function call expression
gam.params = list(data=gam.data)

## set defaults for missing parameters according to R docs:
## see Jon Shuker's inputs specification for reference
gam.defaults = list(family=gaussian(link=identity),
                   subset=NULL,
                   weights=NULL,
                   na.action=options("na.action")[[1]],
                   start=NULL,
                   ea_start=NULL,
                   mu_start=NULL,
                   offset=NULL,
                   control=gam.control(), #note NULL breaks the call to gam
                   method="gam.fit",
                   model=TRUE,
                   x=FALSE,
                   y=FALSE,
                   include_intercept=TRUE,
                   contrasts=NULL)

# plain old parameters
for (paramname in c('formula', 'family', 'na.action', 'control', 'method', 'model', 'x', 'y', 'include_intercept')) {
    if (! is.null(bccvl.params[[paramname]])) {
        gam.params[paramname] = bccvl.params[paramname]
    } else {
        gam.params[paramname] = gam.defaults[paramname]
    }
}

if ( is.null(gam.params$control) ) {
    #Note if this value is NULL gam will fail. Hard coding this for now. TODO discuss with Gerhard and Jon
    gam.params$control=gam.control()
    #Could print a warning
}
# parameters that sholud refer to a column in gam.data
for (paramname in c('start', 'eta_start', 'mu_start', 'subset', 'weights', 'contrasts','offset')) {
    if (! is.null(bccvl.params[[paramname]])) {
        gam.params[paramname] = gam.data[bccvl.params[[paramname]]]
    } else {
        gam.params[paramname] = gam.defaults[paramname]
    }
}

# singular.ok has a different name in bccvl.params
if (! is.null(bccvl.params['singular_ok'])) {
    gam.params['singular.ok'] = bccvl.params$singular_ok
} else {
    gam.params['singular.ok'] = gam.defaults$singular.ok
}



## Build the model
gam.result = gam(formula=formula(gam.params$formula),
                 data=gam.params$data,
                 family=gam.params$family,
                 subset=gam.params$subset,
                 weights=gam.params$weights,
                 na.action=gam.params$na.action[[1]],
                 start=gam.params$start,
                 etastart=gam.params$eta_start,
                 mustart=gam.params$mu_start,
                 offset=gam.params$offset,
                 control=gam.params$control,
                 method=gam.params$method,
                 model=gam.params$model,
                 x=gam.params$x,
                 y=gam.params$y,
                 include_intercept=gam.params$include_intercept,
                 contrasts=gam.params$contrasts)

## Save the result to file

bccvl.save(gam.result, "gam.model.object.RData")

## Save result summary to a text file

sink(file="gam_result_summary.txt")
summary(gam.result)
sink()
