
#' Identify polygon name from the worldHires dataset
#' 
#' @description Check for names of polygons matching the character provided (usually the
#' name of a country). Usefull prior to performing a getPR operation for
#' identifying the polygon name to use in the query.
#' 
#' @details The first letter of polygon names is often capitalized in the worldHires
#' dataset.
#' 
#' @param country character. Usually the name of a country.
#' @return A character list
#' @author Loïc Dutrieux
#' @seealso \code{\link{getPR}}
#' @keywords worldHires
#' @examples
#' 
#' # PLot Texel island (an island on the coast of the Netherlands)
#' list <- getCountryList('Netherlands')
#' list
#' map('worldHires', list[7], exact=TRUE, fill=TRUE)
#' 
#' @export getCountryList
#' 
#' @import utils
#' @import maps
#' 
#' 

getCountryList <- function(country) {
  map <- map('worldHires', plot=FALSE)
  Country <- sprintf('*%s*', country)
  list <- map$names[grepl(glob2rx(Country), map$names)]
  return(list)
}
