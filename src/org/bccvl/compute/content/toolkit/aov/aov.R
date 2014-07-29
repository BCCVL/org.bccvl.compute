
### aov.R ###

## Get the data

aov.data = read.table(bccvl.params$data_table$filename, header=T, sep=",")

## Set up the function call expression
aov.params = list(data=aov.data)

## set defaults for missing parameters according to R docs:
aov.defaults = list(subset=NULL,
                   weights=NULL,
                   contrasts=NULL,
                   na.action=options("na.action")[[1]],
                   qr=TRUE,
                   projections=FALSE)

# plain old parameters
for (paramname in c('formula', 'na.action', 'qr', "projections")) {
    if (! is.null(bccvl.params[[paramname]])) {
        aov.params[paramname] = bccvl.params[paramname]
    } else {
        aov.params[paramname] = aov.defaults[paramname]
    }
}

# parameters that sholud refer to a column in aov.data
for (paramname in c('subset', 'weights', 'contrasts')) {
    if (! is.null(bccvl.params[[paramname]])) {
        aov.params[paramname] = aov.data[bccvl.params[[paramname]]]
    } else {
        aov.params[paramname] = aov.defaults[paramname]
    }
}

## Execute calculations
aov.result = aov(formula=formula(aov.params$formula),
                 data=aov.params$data,
                 subset=aov.params$subset,
                 weights=aov.params$weights,
                 contrasts=aov.params$contrasts,
                 na.action=aov.params$na.action[[1]],
                 qr=aov.params$qr,
                 projections=aov.params$projections,
                 )

## Save the result to file

bccvl.save(aov.result, "aov.model.object.RData")

## Save result summary to a text file

sink(file="aov_result_summary.txt")
summary(aov.result)
sink()

