

erase_valid_half <- function(rast, target_val=1, to_value=NA)
{
    nrow=dim(r)[1]
    ncol=dim(r)[2]

    
    for( c in 1:ncol )
    {
        lat_sum=0
        lat_n=0
        for ( r in 1:nrow )
        {
            val = rast[r,c]
            if (!is.na(val) && val == target_val)
            {
                lat_sum = lat_sum + r
                lat_n = lat_n + 1
            }
            
        }
        if ( lat_n == 0)
            next

        ave_lat = lat_sum/lat_n
        print(ave_lat)
        
        for ( r in 1:nrow )
        {
            val = rast[r,c]
            if (!is.na(val) && val == target_val)
            {
                if ( r  > ave_lat )
                {
                    rast[r,c] = to_value
                }
            }
            
        }
      
    }
}

