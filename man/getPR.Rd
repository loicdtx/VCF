\name{getPR}
\alias{getPR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Query Landsat wrs2 grid by country name
}
\description{
Performs a spatial query on the wrs2 grid using the worldHires dataset.
}
\usage{
getPR(country)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{country}{
character. Usually the name of a country
}
}
\details{
Check the \code{\link{getCountryList}} function to get the right spelling for the polygon name you want to use for the query.
It is recommended to detach rgdal package while using this function.
}
\value{
A list of path, rows, PathRows, and a spatialPolygon object
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Loïc Dutrieux
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getCountryList}}, ~~~
}
\examples{
pr <- getPR('France')
pr$PATH
pr$ROW
pr$PR
map('worldHires', 'France', exact=TRUE)
plot(pr$map, add=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ worldHires }
\keyword{ Landsat }% __ONLY ONE__ keyword per line
