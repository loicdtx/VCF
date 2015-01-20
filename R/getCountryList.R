# Author: Loïc Dutrieux
# June 2013




#' Identify polygon name from the worldHires dataset
#' 
#' Check for names of polygons matching the character provided (usually the
#' name of a country). Usefull prior to performing a getPR operation for
#' identifying the polygon name to use in the query.
#' 
#' The first letter of polygon names is often capitalized in the worldHires
#' dataset.
#' 
#' @param country character. Usually the name of a country.
#' @return A character list
#' @note %% ~~further notes~~
#' @author Loïc Dutrieux
#' @seealso \code{\link{getPR}}
#' @references %% ~put references to the literature/web site here ~
#' @keywords worldHires
#' @examples
#' 
#' # PLot Texel island (an island on the coast of the Netherlands)
#' list <- getCountryList('Netherlands')
#' list
#' map('worldHires', list[7], exact=TRUE, fill=TRUE)
#' 
#' @export getCountryList
getCountryList <- function(country) {
  map <- map('worldHires', plot=FALSE)
  Country <- sprintf('*%s*', country)
  list <- map$names[grepl(glob2rx(Country), map$names)]
  return(list)
}
