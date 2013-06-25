\name{downloadPR}
\alias{downloadPR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automatic download of vcf data from a list of pathrows
}
\description{
Automates the download of Landsat based vcf products from the ftp server, from a list of list of pathrows. Writes status of the download to a log file and recreates the directory organization of the ftp server locally. 
}
\usage{
downloadPR(pr, year, dir, log = NULL, baseURL = "ftp://ftp.glcf.umd.edu/glcf/LandsatTreecover/WRS2/")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pr}{
List or numeric list. Classically the returned object from \code{\link{getPR}}. 
}
  \item{year}{
Numeric or list (i.e.: c(2000, 2005))
}
  \item{dir}{
Character. Directory where to write the downloaded data.
}
  \item{log}{
character. filename of the logfile. If NULL (default), a file 'downloadVCF.log' is created at the root of \code{dir} 
}
  \item{baseURL}{
character.
}
}
\details{
FIles are downloaded only if they have not been downloaded and written at the same location earlier. 
}
\value{
The list of file downloaded, plus eventual warning, or error messages
}
\references{
See \url{http://landcover.org/data/landsatTreecover/}
}
\author{
Loic Dutrieux
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
pr <- getPR('Belize')
pr
downloadPR(pr, year=2000, dir=tempdir())

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Tree cover }
\keyword{ Landsat }% __ONLY ONE__ keyword per line
