
# Get the pseudo aspect ratio of a shape
# Do this by averaging the distance of the great arcs connecting extent extema in the long/lat directions
# 
shape_pseudo_aspect_ratio <- function(shp)
{
    shape_extent=extent(shp)
    extent_matrix=matrix(c(shape_extent@xmin, shape_extent@xmin, shape_extent@xmax,  shape_extent@xmax,
                           shape_extent@ymin, shape_extent@ymax, shape_extent@ymin, shape_extent@ymax),
                           ncol=2)

    west_lat=spDists(extent_matrix[1,,drop=F], extent_matrix[2,,drop=F],longlat=TRUE)
    east_lat=spDists(extent_matrix[3,,drop=F], extent_matrix[4,,drop=F],longlat=TRUE)

    south_lon=spDists(extent_matrix[1,,drop=F], extent_matrix[3,,drop=F],longlat=TRUE)
    north_lon=spDists(extent_matrix[2,,drop=F], extent_matrix[4,,drop=F],longlat=TRUE)

    return ( (south_lon+north_lon)/(west_lat + east_lat) )
}

# auto shape to raster
# figures out a pseudo aspect ratio to auto calculate height
shape_to_raster <- function(shp, 
                            width=256)
{
    library(raster)
    library(rgdal)
    library(sp)

    
    aspect_ratio=shape_pseudo_aspect_ratio(shp)
    height=width/aspect_ratio
    r=raster(ncol=width,nrow=height)
    extent(r) = extent(shp)
    proj4string(r) = proj4string(shp)
    r=rasterize(shp, r)
    r[!is.na(r)] <- 1 
    return(r)
}
