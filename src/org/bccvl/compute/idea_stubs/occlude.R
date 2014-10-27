

occlude <- function(rast, occur, target_val=1)
{
    n=length(occur[[1]])
    vals=extract(rast, occur)
    
    return(occur[vals==target_val,])
}
