
#' Warp individual scenes to a given projection
#' 
#' @description This function allows single scenes to be warped to a given projection. No
#' mosaicking is performed. The function can handle multiple NoData values and
#' can output an alpha layer, which a warped layer containing the NoData values
#' only.
#' 
#' @details Requires gdal to be installed on the system, and the the gdal binary folder
#' should be added to the system path. On windows systems, gdal can be installed
#' via FWTools, OSGeo4W or QGIS. Function behavior is quite different depending on
#' whether single and multiple NoData are provided. \code{gdalwarp}, which is
#' called via a system command does not support multiple NoData values, as a
#' consequence the present function first aggregates all values provided in a
#' single value and does the warping in a second stage.
#' 
#' @param x Character. The filename of the layer to be warped.
#' @param t_srs character. proj4 expression of the output (\code{filename})
#' file.
#' @param nodata numeric. Value that should not get interpolated in the
#' resampling. Can take multiple values (i.e.: \code{c(220, 210, 211)}). No
#' need to specify the nodata value if that one is included in the file header.
#' @param filename character. filename of the output file, with full path.
#' @param res numeric. output resolution.
#' @param method character. resampling method. See
#' \url{http://www.gdal.org/gdalwarp.html}
#' @param alpha Logical. Should an alpha layer be created. Only makes sense in
#' case multiple NoData values are provided.
#' @param run logical. should the warping be executed. If set to false, a
#' gdalwarp command string is generated, but not executed.
#' @param \dots Character. Extra switches passed to \code{gdalwarp}, see
#' \url{http://www.gdal.org/gdalwarp.html}. All arguments have to passed
#' (including defaults) to be abble to use this ellipsis.
#' @return A character, or list of characters, the gdalwarp command(s). If you
#' inted to copy/past it in a terminal, you can use \code{print()}, with
#' \code{quote=FALSE}.
#' @author Loic Dutrieux
#' @references \url{http://www.gdal.org/gdalwarp.html}
#' @keywords gdal Landsat
#' @examples
#' 
#' \dontrun{
#' pr <- getPR('Belize')
#' pr <- pr$PR[1]
#' dir <- tempdir()
#' downloadPR(pr, year=2000, dir=dir)
#' unpackVCF(pr=pr, year=2000, searchDir=dir, dir=sprintf('%s/%s',dir,'extract/'))
#' x <- list.files(sprintf('%s/%s',dir,'extract/'), full.names=TRUE)
#' filename <- sprintf('%s.tif', rasterTmpFile())
#' warp(x=x, t_srs='+proj=laea +lat_0=-10 +lon_0=-70 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs', nodata = c(200, 210, 211, 220), filename=filename)
#' a <- raster(filename)
#' 
#' # This function is intended to be applied to several scenes, as a way to prepare them before mosaicking
#' # The example below performs warping over three files and subsequently mosaic them
#' 
#' pr <- getPR('Belize')
#' dir <- tempdir()
#' downloadPR(pr, year=2000, dir=dir)
#' unpackVCF(pr=pr, year=2000, searchDir=dir, dir=sprintf('%s/%s',dir,'extract/'))
#' x <- list.files(sprintf('%s/%s',dir,'extract/'), full.names=TRUE)
#' 
#' wrap <- function(x) {
#'   filename <- filename <- sprintf('%s_warp.tif', x)
#'   w <- warp(x=x, t_srs='+proj=laea +lat_0=-10 +lon_0=-70 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs', nodata = c(200, 210, 211, 220), filename=filename)
#'   return(w)
#' }
#' 
#' mclapply(X=x, FUN=wrap, mc.cores=3)
#' 
#' x2 <- list.files(sprintf('%s/%s',dir,'extract/'), pattern=glob2rx('*warp*'), full.names=TRUE)
#' 
#' x3 <- lapply(X=x2, FUN=raster)
#' x3$fun <- mean
#' x3$filename <- sprintf('%s/%s',dir,'extract/mosaic_belize.tif')
#' 
#' do.call(raster::mosaic, x3)
#' plot(r <- raster(x3$filename))
#' 
#' }
#' 
#' @export warp
#' 
#' @import raster
#' @import rgdal
#' 

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
      warpArgs <- list(sprintf('gdalwarp -tap --config GDAL_CACHEMAX 500 -wm 500 -t_srs \"%s\" -tr %d %d -r %s -srcnodata %d -dstnodata 255', t_srs, res, res, method, nodata), ..., x, filename)
    } else if (length(nodata) > 1) {
      r <- raster::raster(x) 
      # An intermediary step is required 
      
      if (alpha) {
        tmpalpha <- raster::rasterTmpFile()
        tmpalpha <- sprintf('%s.tif', tmpalpha)
        alphaName <- sprintf('%s_alpha%s', substr(filename, 1, nchar(filename) - nchar(raster::extension(filename))), raster::extension(filename))
        raster::calc(x=r, fun=x2alpha, filename=tmpalpha, datatype='INT1U')
        alphaw <- sprintf('gdalwarp -tap --config GDAL_CACHEMAX 500 -wm 500 -t_srs \"%s\" -tr %d %d %s %s', t_srs, res, res, tmpalpha, alphaName) #warp string for alpha layer (nearest neighbour)
      }
      tmp <- fun(r) #tmp is the filename of the file to be warped
      warpArgs <- list(sprintf('gdalwarp -tap --config GDAL_CACHEMAX 500 -wm 500 -t_srs \"%s\" -tr %d %d -r %s -dstnodata 255', t_srs, res, res, method), ..., tmp, filename)
      
    }
  } else {
    warpArgs <- list(sprintf('gdalwarp -tap --config GDAL_CACHEMAX 500 -wm 500 -t_srs \"%s\" -tr %d %d -r %s -dstnodata 255', t_srs, res, res, method), ..., x, filename)
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
