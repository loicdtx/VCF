# Author: Loïc Dutrieux
# June 2013
getCountryList <- function(country) {
  map <- map('worldHires', plot=FALSE)
  Country <- sprintf('*%s*', country)
  list <- map$names[grepl(glob2rx(Country), map$names)]
  return(list)
}