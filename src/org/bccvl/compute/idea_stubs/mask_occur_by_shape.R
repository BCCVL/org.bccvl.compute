
# shape is assumed to be SpatialPolygonsDataFrame but can be any object handled by rgeo::gContains
# occur is an Nx2 data.frame with $lon $lat column entries
mask.location.by.shape <- function(shape, occur) 
{
    filtered = data.frame(lon=c(), lat=c())

    for( j in 1:length(occur[[1]]))
    {
        point <- occur[j,]
        sp2   <- SpatialPoints(point,proj4string=CRS(proj4string(shape)))
        if ( gContains(shape, sp2) )
        {
            filtered=rbind(filtered, point)
        }
    }
    return (filtered)
}
