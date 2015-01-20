
#' Query Landsat wrs2 grid by country name
#' 
#' @description Performs a spatial query on the wrs2 grid using the worldHires dataset.
#' 
#' @details Check the \code{\link{getCountryList}} function to get the right spelling
#' for the polygon name you want to use for the query.
#' 
#' @param country character. Usually the name of a country
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


getPR <- function(country) {
  #Get the wrs grid
  data(wrs2) # wrs2
  
  extm <- map("worldHires", country, plot=FALSE, fill=TRUE, col='transparent', exact=TRUE) #For some reason the fill and col arguments seem to be important ...
  IDs <- sapply(strsplit(extm$names, ":"), function(x) x[1])
  exts <- map2SpatialPolygons(extm, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#   if(!identical(proj4string(exts), proj4string(wrs2))) {
#       exts <- spTransform(exts, CRS(proj4string(wrs2)))
#   }
  
  # Perform the Spatial Query'
  x <- wrs2[exts,]
  list <- list(PR=x$PR, PATH=x$PATH, ROW=x$ROW, map=x)
  return(list)  
}




