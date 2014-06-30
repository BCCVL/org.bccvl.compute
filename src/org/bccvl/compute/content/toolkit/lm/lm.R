
### lm.R ###


## Get the data

lm.data = read.table(bccvl.params$data_table$filename, header=T, sep=",")

## Set up the function call expression
lm.params = list(data=lm.data)

# scalar parameters
for (paramname in c('formula', 'na.action', 'method', 'model', 'x', 'y', 'qr')) {
    if (! is.null(bccvl.params[[paramname]])) {
        lm.params[paramname] = bccvl.params[paramname]
    }
}

# parameters that sholud refer to a column in lm.data
for (paramname in c('subset', 'weights', 'contrasts','offset')) {
    if (! is.null(bccvl.params[[paramname]])) {
        lm.params[paramname] = lm.data[bccvl.params[[paramname]]]
    }
}

# TODO: bccvl.params$other_params ... x, y, offset, method, tol, singular.ok

## Run the regression

lm.result = do.call(lm, lm.params)

## Save the result to file

bccvl.save(lm.result, "lm.model.object.RData")

## Save result summary to a text file

sink(file="lm_result_summary.txt")
summary(lm.result)
sink()
