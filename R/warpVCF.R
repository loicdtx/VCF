warpVCF <- function(x, t_srs, nodata, filename, res=30, method='bilinear', mc.cores=1, run=TRUE, ...) {
  # x is a list of characters (the filenames)
  
  if(length(nodata) > 1) { # More than one nodata value is provided, an intermediary step is required in that case    
    
    fun <- function(i){
      tmp <- rasterTmpFile()
      tmp <- sprintf('%s.tif', tmp)    
      r <- raster::raster(i)
      clean <- function(x) { # replace all nodata provided by 220
        x[x %in% nodata] <- 220
        return(x)
      }
      raster::calc(x=r, fun=clean, filename=tmp)
      return(tmp)
    }
    prelist <- mclapply(X=x, FUN=fun, mc.cores=mc.cores)
    x <- simplify2array(prelist) # FOrmat ouput of lapply
    nodata <- 220
  }
  
  t_srs <- sprintf('\"%s\"', t_srs) #Add extra quotes around the proj4 expression
  if (.Platform$OS=='unix') {
    strList <- list(sprintf('gdalwarp -t_srs %s -tr %d %d -r %s -srcnodata %d', t_srs, res, res, method, nodata), ..., paste(x, collapse=' '), filename)
    warpstr <- paste(strList, collapse=' ')
    if (run) {
      system(warpstr)
    }
  } else {
    warning('Non unix platform, may not work ... yet, contact me if you are interested in that feature. Loic')
    strList <- list(sprintf('gdalwarp -t_srs %s -tr %d %d -r %s -srcnodata %d', t_srs, res, res, method, nodata), ..., paste(x, collapse=' '), filename)
    warpstr <- paste(strList, collapse=' ')
    if (run) {
      system(warpstr)
    }
  }
  return(warpstr)
}