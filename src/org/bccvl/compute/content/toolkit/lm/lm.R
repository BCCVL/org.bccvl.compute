
### lm.R ###


## Get the data

lm.data = read.table(bccvl.params$data[1], header=T, sep=",")


## Set up the function call expression

lm.call = paste(
        "lm(",
        bccvl.params$formula[1], ",",
        bccvl.params$data_table[1], ",",
        "subset=", bccvl.params$subset[1], ",",
        "weights=", bccvl.params$weights[1], ",",
        "na.action=", bccvl.params$na_action[1], ",",
        "method=\'", bccvl.params$method[1], "\',",
        "model=", bccvl.params$model[1], ",",
        "x=", bccvl.params$x[1], ",",
        "y=", bccvl.params$y[1], ",",
        "qr=", bccvl.params$qr[1], ",",
        "singular.ok=", bccvl.params$singular.ok[1], ",",
        "contrasts=", bccvl.params$contrasts[1], ",",
        "offset=", bccvl.params$offset[1],
        sep="")

if (bccvl.params$other_args[1] != "NULL") {
  lm.call = paste( lm.call, bccvl.params$other_args[1], sep="")
  }

lm.call = paste( lm.call, ")", sep="" )


## Run the regression

lm.result =
  eval( parse( text=lm.call ) )


## Save the result to file

bccvl.save(lm.result, "lm.model.object.RData")


## Save result summary to a text file

sink(file="lm_result_summary.txt")
summary(lm.result)
sink()


