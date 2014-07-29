
### manova.R ###

## Get the data

manova.data = read.table(bccvl.params$data_table$filename, header=T, sep=",")

## Set up the function call expression
manova.params = list(data=manova.data)

## set defaults for missing parameters according to R docs:
manova.defaults = list(subset=NULL,
                   weights=NULL,
                   contrasts=NULL,
                   na.action=options("na.action")[[1]],
                   qr=TRUE,
                   projections=FALSE)

# plain old parameters
for (paramname in c('formula', 'na.action', 'qr', "projections")) {
    if (! is.null(bccvl.params[[paramname]])) {
        manova.params[paramname] = bccvl.params[paramname]
    } else {
        manova.params[paramname] = manova.defaults[paramname]
    }
}

# parameters that sholud refer to a column in manova.data
for (paramname in c('subset', 'weights', 'contrasts')) {
    if (! is.null(bccvl.params[[paramname]])) {
        manova.params[paramname] = manova.data[bccvl.params[[paramname]]]
    } else {
        manova.params[paramname] = manova.defaults[paramname]
    }
}

## Execute calculations
manova.result = manova(formula=formula(manova.params$formula),
                 data=manova.params$data,
                 subset=manova.params$subset,
                 weights=manova.params$weights,
                 contrasts=manova.params$contrasts,
                 na.action=manova.params$na.action[[1]],
                 qr=manova.params$qr,
                 projections=manova.params$projections,
                 )

## Save the result to file

bccvl.save(manova.result, "manova.model.object.RData")

## Save result summary to a text file

sink(file="manova_result_summary.txt")
summary(manova.result)
sink()

