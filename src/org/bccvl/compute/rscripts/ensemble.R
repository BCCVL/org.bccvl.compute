#### ensemble.R ####


## Build a raster stack from .tif files selected by the user
rs = stack(lapply(bccvl.params$datasets, function(x) x$filename))
exp_title = bccvl.params$title
sdm_projections = NULL
threshold.median = NULL
if (length(bccvl.params$sdm_projections) > 0) {
	sdm_projections = stack(lapply(bccvl.params$sdm_projections, function(x) x$filename))
	thresholds = bccvl.params$thresholds
	threshold.median = median(unlist(thresholds))
	sdm_projection.mean = mean(sdm_projections)
}

output_dir = bccvl.env$outputdir

## Generate ensemble analyses

r.mean = mean(rs)
writeRaster( r.mean, filename=file.path( output_dir, paste0('ensemble_mean_', exp_title, '.tif')),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.max = max(rs)
writeRaster( r.max, filename=file.path( output_dir, paste0('ensemble_max_', exp_title, '.tif')),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.min = min(rs)
writeRaster( r.min, filename=file.path( output_dir, paste0('ensemble_min_', exp_title, '.tif')),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.var = calc( rs, fun=function(x){var(x)} )
writeRaster( r.var, filename=file.path( output_dir, paste0('ensemble_variance_', exp_title, '.tif')),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.q0p05 = calc( rs, fun=function(x){quantile(x,probs=0.05,na.rm=TRUE)} )
writeRaster( r.q0p05, filename=file.path( output_dir, paste0('ensemble_q0p05_', exp_title, '.tif')),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.q0p1 = calc( rs, fun=function(x){quantile(x,probs=0.1,na.rm=TRUE)} )
writeRaster( r.q0p1, filename=file.path( output_dir, paste0('ensemble_q0p1_', exp_title, '.tif')),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.q0p5 = calc( rs, fun=function(x){quantile(x,probs=0.5,na.rm=TRUE)} )
writeRaster( r.q0p5, filename=file.path( output_dir, paste0('ensemble_q0p5_', exp_title, '.tif')),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.q0p9 = calc( rs, fun=function(x){quantile(x,probs=0.9,na.rm=TRUE)} )
writeRaster( r.q0p9, filename=file.path( output_dir, paste0('ensemble_q0p9_', exp_title, '.tif')),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

r.q0p95 = calc( rs, fun=function(x){quantile(x,probs=0.95,na.rm=TRUE)} )
writeRaster( r.q0p95, filename=file.path( output_dir, paste0('ensemble_q0p95_', exp_title, '.tif')),
	format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

# generate species range change metric and summary only if both CC and SDM projections are available.
if (!is.null(sdm_projections)) {
	changefilepath = file.path(output_dir, paste0('ensemble_rangechange_', exp_title, '.tif'))
	proj_rasters = list(sdm_projection.mean, r.mean)
	bccvl.generateSpeciesRangeChangeMetric(proj_rasters, threshold.median, changefilepath)
}