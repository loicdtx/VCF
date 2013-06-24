\name{getCountryList}
\alias{getCountryList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify polygon name from the worldHires dataset
}
\description{
Check for names of polygons matching the character provided (usually the name of a country). Usefull prior to performing a getPR operation for identifying the polygon name to use in the query.
}
\usage{
getCountryList(country)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{country}{
character. Usually the name of a country.
}
}
\details{
The first letter of polygon names is often capitalized in the worldHires dataset. 
}
\value{
A character list
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
\code{\link{getPR}}
}
\examples{
# PLot Texel island (an island on the coast of the Netherlands)
list <- getCountryList('Netherlands')
list
map('worldHires', list[7], exact=TRUE, fill=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ worldHires }
