

pixel_to_lon_lat <- function(mask, r, c)
{
    ext=extent(rasterFromCells(mask, cellFromRowCol(mask, rownr=r,colnr=c), values=T))
    return(c((ext@xmin+ext@xmax)/2.0, (ext@ymin+ext@ymax)/2.0))
}

random_valid_from_mask <- function(mask, target_val = 1)
{
    nrow=dim(mask)[1]
    ncol=dim(mask)[2]

    selected_points = sample(nrow*ncol, nrow*ncol)

    for ( i in 1:length(selected_points) )
    {
        r=floor(selected_points[i] / ncol)
        c=selected_points[i] %% ncol
        val = mask[r,c]
        if ( !is.na(val) && !is.null(val) && val==target_val )
        {
            return(pixel_to_lon_lat(mask, r, c))
        }
    }

    return(NULL)

}

n_random_valid_from_mask <- function(mask, n, target_val = 1)
{
    df = data.frame(lon=numeric(n), lat=numeric(n))
    for( i in 1:n )
    {
        point=random_valid_from_mask(mask)
        if ( is.null(point) )
        {
            stop("Got null point")
        }
        df$lon[i] = point[1]
        df$lat[i] = point[2]
    }
    return(df)
}

random_row_from_mask <- function(mask)
{
    ncol=dim(mask)[2]
    c=sample(ncol,1)
    return(c)    
}

random_col_from_mask <- function(mask)
{
    nrow=dim(mask)[1]
    r=sample(nrow,1)
    return(r)    
}

random_from_mask <- function(mask)
{
    return(pixel_to_lon_lat(mask, random_col_from_mask(mask), random_row_from_mask(mask)))
}

n_random_from_mask <- function(mask, n)
{
    vals=numeric(n)
    fun <- function(x) { return(random_from_mask(mask)) }
    vals=lapply(vals, fun)
    print(vals)
    df = data.frame(lon=numeric(n), lat=numeric(n))
    lat_index=1
    lon_index=1
    fun_lat <- function(x) { val=vals[[lat_index]][2]; lat_index <<- lat_index+1; return(val) }
    fun_lon <- function(x) { val=vals[[lon_index]][1]; lon_index <<- lon_index+1; return(val) }
    df$lon=lapply(df$lon, fun_lon)
    df$lat=lapply(df$lat, fun_lat)
    return(df)
}



