\name{unpackVCF}
\alias{unpackVCF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Uncompress the .gz archives of the vcf data
}
\description{
Recursively searches through the directories and unpack the queried archives.
}
\usage{
unpackVCF(pr, year, searchDir, dir = NULL, mc.cores = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pr}{
List or numeric list. Classically the returned object from \code{\link{getPR}}. 
}
  \item{year}{
Numeric or list (i.e.: c(2000, 2005))
}
  \item{searchDir}{
character. Root directory of where the archive are stored
}
  \item{dir}{
character or \code{NULL}, if \code{NULL} (default), data are unpacked in the directories containing the archive. If a directory is provided, all data are unpacked in that directory.
}
  \item{mc.cores}{
numeric. For parallel implementation, number of workers.
}
}
\details{
%
}
\value{
A list of filenames and potential warning/error messages
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Loic Dutrieux
}
\note{
%%  ~~further notes~~
}
\section{Warning }{
For parallel implementation, see warning section of \code{\link{mclapply}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
pr <- getPR('Belize')
pr
dir = tempdir()
downloadPR(pr, year=2000, dir=dir)
unpackVCF(pr=pr, year=2000, searchDir=dir, dir=sprintf('\%s/\%s',dir,'extract/'))
list.files(sprintf('\%s/\%s',dir,'extract/'))

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Landsat }
\keyword{ .gz }% __ONLY ONE__ keyword per line
