

bccvl.maps.lines.Australia <- function()
{
    # based on from http://ifgi.uni-muenster.de/~epebe_01/Aufbaukurs/R/slides_R.pdf

    library(maps)
    library(mapdata)
    library(maptools)

    # thumb in the wind limits - feel free to tinker
    lon_limits=c(112, 157)
    lat_limits=c(-45, -8)

    wrld = map("world", interior = FALSE, plot = FALSE,  xlim = lon_limits , ylim = lat_limits )

    # still figuring out which is the best crs for Australia
    crs=CRS("+init=epsg:3577")
    #crs=CRS("+init=epsg:3035")) # seemed okay - probably better for the UK though
    
    wrld.sp = map2SpatialLines(wrld, proj4string = crs )

	return (wrld.sp)
}

bccvl.maps.plot.Australia <- function()
{
	oz_lines=bccvl.maps.lines.Australia()
    plot(oz_lines, axes = TRUE, col = "grey", lwd=10)
}
