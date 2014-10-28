

block_raster <- function(rast, height_ratio=0.5, width_ratio=0.5)
{
    rcopy=rast
    nrow=dim(rast)[1]
    ncol=dim(rast)[2]

    
    for( r in 1:nrow )
    {
        for ( c in 1:ncol )
        {
            if ( r/nrow < width_ratio || 
                 c/ncol < height_ratio)
            {
                rcopy[r,c] = 1
            }
            else
            {
                rcopy[r,c] = NA
            }
        }
    }
    return(rcopy)
}
