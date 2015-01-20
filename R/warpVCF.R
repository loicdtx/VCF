# Loic Dutrieux
# July 2013

# TODO() Implement a switch to warp without mosaicking
# TODO() Intermediary should as much as possible be writted to integer (should be flexible based on the data range)





#' Warping utility
#' 
#' @description Warps (mosaic, reproject, resample), raster files to a projection set by the
#' user. The function works by calling \code{gdalwarp}, which needs to be
#' installed on the system.
#' 
#' @details Requires gdal to be installed on the system, and the the gdal binary folder
#' should be added to the system path. On windows systems, gdal can be install
#' via FWTools, OSGeo4W or QGIS.
#' 
#' @param x character or list of character, the filenames of files to be
#' warpped. The list can easily be retrieved using a call such as
#' \code{list.files('/path/to/data/', full.names=TRUE)}
#' @param t_srs character. proj4 expression of the output (\code{filename})
#' file.
#' @param nodata numeric. Value that should not get interpolated in the
#' resampling. Can take multiple values (i.e.: \code{c(220, 210, 211)}). No
#' need to specify the nodata value if that one is included in the file header.
#' @param filename character. filename of the output file, with full path.
#' @param res numeric. output resolution.
#' @param method character. resampling method. See
#' \url{http://www.gdal.org/gdalwarp.html}
#' @param mc.cores Numeric. Only relevant if \code{length(nodata) > 1}. Number
#' of workers.
#' @param run logical. should the warping be executed. If set to false, a
#' gdalwarp command string is generated, but not executed.
#' @param \dots Extra switches passed to \code{gdalwarp}, see
#' \url{http://www.gdal.org/gdalwarp.html}.
#' @return A character, the gdalwarp command. If you inted to copy/past it in a
#' terminal, you can use \code{print()}, with \code{quote=FALSE}.
#' @section Warning : For parallel implementation, see warning section of
#' \code{\link{mclapply}}
#' @author Loic Dutrieux
#' @references \url{http://www.gdal.org/gdalwarp.html}
#' @keywords gdal landsat
#' @examples
#' 
#' \dontrun{
#' pr <- getPR('Belize')
#' pr
#' dir = tempdir()
#' downloadPR(pr, year=2000, dir=dir)
#' unpackVCF(pr=pr, year=2000, searchDir=dir, dir=sprintf('%s/%s',dir,'extract/'))
#' x <- list.files(sprintf('%s/%s',dir,'extract/'), full.names=TRUE)
#' filename <- sprintf('%s.tif', rasterTmpFile())
#' warpVCF(x=x, t_srs='+proj=laea +lat_0=-10 +lon_0=-70 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs', nodata = c(200, 210, 211, 220), filename=filename, '-multi')
#' a <- raster(filename)
#' }
#' 
#' @export warpVCF
#' 
#' @import raster
#' @import parallel
#' 
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
