# Loic Dutrieux
# July 2013

warp <- function(x, t_srs, nodata=NULL, filename, res=30, method='bilinear', alpha=FALSE, run=TRUE, ...) {
  
  # Subfunctions
  clean <- function(x) { # replace all nodata provided by NA
    x[x %in% nodata] <- NA
    return(x)            
  }
  
  fun <- function(i){ # Creates a file with an aggregated nodata value
    tmp <- raster::rasterTmpFile()
    tmp <- sprintf('%s.tif', tmp)      
    raster::calc(x=i, fun=clean, filename=tmp, datatype='INT1U') # 
    return(tmp)
  }
  
  x2alpha <- function(x) { # Create a tmp
    alphaLayer <- x
    alphaLayer[!(alphaLayer %in% nodata)] <- NA
    return(alphaLayer)
  }
  
  
  
  
  # Intermediary steps for various nodata options (NULL, length==1, length>1)  
  if (!is.null(nodata)) {
    if (length(nodata) == 1) {
      warpArgs <- list(sprintf('gdalwarp --config GDAL_CACHEMAX 500 -wm 500 -t_srs \"%s\" -tr %d %d -r %s -srcnodata %d', t_srs, res, res, method, nodata), ..., x, filename)
    } else if (length(nodata) > 1) {
      r <- raster::raster(x) 
      # An intermediary step is required 
      
      if (alpha) {
        tmpalpha <- raster::rasterTmpFile()
        tmpalpha <- sprintf('%s.tif', tmpalpha)
        alphaName <- sprintf('%s_alpha%s', substr(filename, 1, nchar(filename) - nchar(raster::extension(filename))), raster::extension(filename))
        raster::calc(x=r, fun=x2alpha, filename=tmpalpha, datatype='INT1U')
        alphaw <- sprintf('gdalwarp --config GDAL_CACHEMAX 500 -wm 500 -t_srs \"%s\" -tr %d %d %s %s', t_srs, res, res, tmpalpha, alphaName) #warp string for alpha layer (nearest neighbour)
      }
      tmp <- fun(r) #tmp is the filename of the file to be warped
      warpArgs <- list(sprintf('gdalwarp --config GDAL_CACHEMAX 500 -wm 500 -t_srs \"%s\" -tr %d %d -r %s', t_srs, res, res, method), ..., tmp, filename)
      
    }
  } else {
    warpArgs <- list(sprintf('gdalwarp --config GDAL_CACHEMAX 500 -wm 500 -t_srs \"%s\" -tr %d %d -r %s', t_srs, res, res, method), ..., x, filename)
  }
  
  # build warp string
  w <- paste(warpArgs, collapse=' ')
  
  
  #run it
  if (run) {
    system(w)
    if (alpha) {
      system(alphaw)
    }
  }
  
  # Return string(s)
  if (alpha) {
    return(list(data=w, alpha=alphaw))
  } else {
    return(w)
  }
  
  
}