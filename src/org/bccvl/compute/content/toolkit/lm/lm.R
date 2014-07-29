
### lm.R ###


## Get the data

lm.data = read.table(bccvl.params$data_table$filename, header=T, sep=",")

## Set up the function call expression
lm.params = list(data=lm.data)

## set defaults for missing parameters according to R docs:
lm.defaults = list(subset=NULL,
                   weights=NULL,
                   na.action=options("na.action")[[1]],
                   method="qr",
                   model=TRUE,
                   x=FALSE,
                   y=FALSE,
                   qr=TRUE,
                   singular.ok=TRUE,
                   contrasts=NULL,
                   offset=NULL)

# scalar parameters
for (paramname in c('formula', 'na.action', 'method', 'model', 'x', 'y', 'qr')) {
    if (! is.null(bccvl.params[[paramname]])) {
        lm.params[paramname] = bccvl.params[paramname]
    } else {
        lm.params[paramname] = lm.defaults[paramname]
    }
}

# parameters that sholud refer to a column in lm.data
for (paramname in c('subset', 'weights', 'contrasts','offset')) {
    if (! is.null(bccvl.params[[paramname]])) {
        lm.params[paramname] = lm.data[bccvl.params[[paramname]]]
    } else {
        lm.params[paramname] = lm.defaults[paramname]
    }
}

# singular.ok has a different name in bccvl.params
if (! is.null(bccvl.params['singular_ok'])) {
    lm.params['singular.ok'] = bccvl.params$singular_ok
} else {
    lm.params['singular.ok'] = lm.defaults$singular.ok
}

## Run the regression
lm.result = lm(formula=lm.params$formula,
               data=lm.params$data,
               subset=lm.params$subset,
               weights=lm.params$weigths,
               na.action=lm.params$na.action[[1]],
               method=lm.params$method,
               model=lm.params$model,
               x=lm.params$x,
               y=lm.params$y,
               qr=lm.params$qr,
               singular.ok=lm.params$singular.ok,
               contrasts=lm.params$contrasts,
               offset=lm.params$offset
               )

## Save the result to file

bccvl.save(lm.result, "lm.model.object.RData")

## Save result summary to a text file

sink(file="lm_result_summary.txt")
summary(lm.result)
sink()
