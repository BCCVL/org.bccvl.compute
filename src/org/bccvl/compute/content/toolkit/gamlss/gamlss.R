
### gamlss.R ###

# TODO: verify with Gerhard that this is the appropriate place to load this
#install.packages("gamlss")
library("gamlss")

## Get the data

gamlss.data = read.table(bccvl.params$data_table$filename, header=T, sep=",")

## Set up the function call expression
gamlss.params = list(data=gamlss.data)


## set defaults for missing parameters according to R docs:
## see Jon Shuker's inputs specification for reference
gamlss.defaults = list(sigma_formula=~1,
                       nu_formula=~1,
                       tau_formula=~1,
                       family=NO(),
                       weights=NULL,
                       contrasts=NULL,
                       method="RS()",
                       start_from=NULL,
                       mu_start=NULL,
                       sigma_start=NULL,
                       nu_start=NULL,
                       tau_start=NULL,
                       mu_fix=FALSE,
                       sigma_fix=FALSE,
                       nu_fix=FALSE,
                       tau_fix=FALSE)

# plain old parameters
for (paramname in c('formula',
                    'sigma_formula', 
                    'nu_formula',
                    'tau_formula',
                    'family',
                    'method',
                    'start_from',
                    'mu_start',
                    'sigma_start',
                    'nu_start',
                    'sigma_start',
                    'nu_start',
                    'tau_start',
                    'mu_fix',
                    'sigma_fix',
                    'nu_fix',
                    'tau_fix' )) {
    if (! is.null(bccvl.params[[paramname]])) {
        gamlss.params[paramname] = bccvl.params[paramname]
    } else {
        gamlss.params[paramname] = gamlss.defaults[paramname]
    }
}

# parameters that should refer to a column in gamlss.data
for (paramname in c('weights', 'contrasts')) {
    if (! is.null(bccvl.params[[paramname]])) {
        gamlss.params[paramname] = gamlss.data[bccvl.params[[paramname]]]
    } else {
        gamlss.params[paramname] = gamlss.defaults[paramname]
    }
}

## Build the model
# TODO sort of the model issue without using eval
#method=gamlss.params$method,

gamlss.result = gamlss(formula=formula(gamlss.params$formula),
                       sigma.formula=gamlss.params$sigma_formula,
                       nu.formula=gamlss.params$nu_formula,
                       tau.formula=gamlss.params$tau_formula,
                       family=gamlss.params$family,
                       data=gamlss.params$data,
                       weights=gamlss.params$weights,
                       contrasts=gamlss.params$contrasts,
                       start.from=gamlss.params$start_from,
                       method=gamlss.RS(),
                       mu.start=gamlss.params$mu_start,
                       sigma.start=gamlss.params$sigma_start,
                       nu.start=gamlss.params$nu_start,
                       tau.start=gamlss.params$tau_start,
                       mu.fix=gamlss.params$mu_fix,
                       sigma.fix=gamlss.params$sigma_fix,
                       nu.fix=gamlss.params$nu_fix,
                       tau.fix=gamlss.params$tau_fix)
            

## Save the result to file

bccvl.save(gamlss.result, "gamlss.model.object.RData")

## Save result summary to a text file

sink(file="gamlss_result_summary.txt")
summary(gamlss.result)
sink()
