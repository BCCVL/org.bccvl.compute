#### ensemble.R ####


## Build a raster stack from .tif files selected by the user
rs = stack(lapply(bccvl.params$datasets, function(x) x$filename))

output_dir = bccvl.env$outputdir

## Generate ensemble analyses

r.mean = mean(rs)
writeRaster( r.mean, filename=file.path( output_dir, 'ensemble_mean.tif' ),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.max = max(rs)
writeRaster( r.max, filename=file.path( output_dir, 'ensemble_max.tif' ),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.min = min(rs)
writeRaster( r.min, filename=file.path( output_dir, 'ensemble_min.tif' ),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.var = calc( rs, fun=function(x){var(x)} )
writeRaster( r.var, filename=file.path( output_dir, 'ensemble_variance.tif' ),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.q0p05 = calc( rs, fun=function(x){quantile(x,probs=0.05,na.rm=TRUE)} )
writeRaster( r.q0p05, filename=file.path( output_dir, 'ensemble_q0p05.tif' ),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.q0p1 = calc( rs, fun=function(x){quantile(x,probs=0.1,na.rm=TRUE)} )
writeRaster( r.q0p1, filename=file.path( output_dir, 'ensemble_q0p1.tif' ),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.q0p5 = calc( rs, fun=function(x){quantile(x,probs=0.5,na.rm=TRUE)} )
writeRaster( r.q0p5, filename=file.path( output_dir, 'ensemble_q0p5.tif' ),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.q0p9 = calc( rs, fun=function(x){quantile(x,probs=0.9,na.rm=TRUE)} )
writeRaster( r.q0p9, filename=file.path( output_dir, 'ensemble_q0p9.tif' ),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.q0p95 = calc( rs, fun=function(x){quantile(x,probs=0.95,na.rm=TRUE)} )
writeRaster( r.q0p95, filename=file.path( output_dir, 'ensemble_q0p95.tif' ),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)
