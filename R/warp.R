# Loic Dutrieux
# July 2013

warp <- function(x, t_srs, nodata=NULL, filename, res=30, method='bilinear', run=TRUE, ...) {
  # Intermediary steps for various nodata options (NULL, length==1, length>1)
  if (!is.null(nodata)) {
    if (length(nodata) == 1) {
      warpArgs <- list(sprintf('gdalwarp --config GDAL_CACHEMAX 500 -wm 500 -t_srs %s -tr %d %d -r %s -srcnodata %d', t_srs, res, res, method, nodata), ..., x, filename)
    } else if (length(nodata) > 1) {
      # An intermediary step is required
    }
  } else {
    warpArgs <- list(sprintf('gdalwarp --config GDAL_CACHEMAX 500 -wm 500 -t_srs %s -tr %d %d -r %s', t_srs, res, res, method), ..., x, filename)
  }
  
  # build warp string
  w <- paste(warpArgs, collapse=' ')
  return(w)
  
  #run it
  if (run) {
    system(w)
  }
}