# Loic Dutrieux
# July 2013

# TODO() Implement a switch to warp without mosaicking
# TODO() Intermediary should as much as possible be writted to integer (should be flexible based on the data range)

warpVCF <- function(x, t_srs, nodata=NULL, filename, res=30, method='bilinear', mc.cores=1, run=TRUE, ...) {
  # x is a list of characters (the filenames)
  
  if(!is.null(nodata)) { # More than one nodata value is provided, an intermediary step is required in that case    
    
    fun <- function(i){
      tmp <- raster::rasterTmpFile()
      tmp <- sprintf('%s.tif', tmp)    
      r <- raster::raster(i)
      clean <- function(x) { # replace all nodata provided by NA
        x[x %in% nodata] <- NA
        return(x)
      }
      raster::calc(x=r, fun=clean, filename=tmp, datatype='INT1U') # 
      return(tmp)
    }
    prelist <- mclapply(X=x, FUN=fun, mc.cores=mc.cores)
    x <- simplify2array(prelist) # FOrmat ouput of lapply
    nodata <- 220
  }
  
  t_srs <- sprintf('\"%s\"', t_srs) #Add extra quotes around the proj4 expression
  if (.Platform$OS=='unix') {
    strList <- list(sprintf('gdalwarp --config GDAL_CACHEMAX 500 -wm 500 -t_srs %s -tr %d %d -r %s', t_srs, res, res, method), ..., paste(x, collapse=' '), filename)
    warpstr <- paste(strList, collapse=' ')
    if (run) {
      system(warpstr)
    }
  } else {
    warning('Non unix platform, may not work ... yet, contact me if you are interested in that feature. Loic')
    strList <- list(sprintf('gdalwarp --config GDAL_CACHEMAX 500 -wm 500 -t_srs %s -tr %d %d -r %s', t_srs, res, res, method), ..., paste(x, collapse=' '), filename)
    warpstr <- paste(strList, collapse=' ')
    if (run) {
      system(warpstr)
    }
  }
  return(warpstr)
}