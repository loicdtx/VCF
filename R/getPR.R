
#' Query Landsat wrs2 grid by country name or location
#' 
#' @description Performs a spatial query on the wrs2 grid using the worldHires dataset or an object of class Spatial*
#' 
#' @details Check the \code{\link{getCountryList}} function to get the right spelling
#' for the polygon name you want to use for the query.
#' 
#' @param location character (Usually the name of a country) or an object of class Spatial*
#' @return A list of path, rows, PathRows, and a spatialPolygon object
#' @author Loïc Dutrieux
#' @seealso \code{\link{getCountryList}}
#' @keywords worldHires Landsat
#' @examples
#' 
#' pr <- getPR('France')
#' pr$PATH
#' pr$ROW
#' pr$PR
#' map('worldHires', 'France', exact=TRUE)
#' plot(pr$map, add=TRUE)
#' 
#' 
#' @export getPR
#' 
#' @import maps
#' @import mapdata
#' @import maptools
#' @import rgeos
#' @import sp


getPR <- function(location) {
  #Get the wrs grid
  data(wrs2) # wrs2
  if(is.character(location)) {
      extm <- map("worldHires", location, plot=FALSE, fill=TRUE, exact=TRUE)
      IDs <- sapply(strsplit(extm$names, ":"), function(x) x[1])
      exts <- map2SpatialPolygons(extm, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  } else if (inherits(location, 'Spatial')) {
      exts <- spTransform(location, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  

  # Perform the Spatial Query'
  i <- gIntersects(exts, wrs2, byid = TRUE)
  x <- wrs2[i[,1],]
  list <- list(PR=x$PR, PATH=x$PATH, ROW=x$ROW, map=x)
  return(list)  
}




