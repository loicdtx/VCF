\name{cloudFill}
\alias{cloudFill}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cloud filling utilities
}
\description{
Uses the MODIS VCF product to fill the remaining cloud gaps in the Landsat VCF product. The function takes care of downloading the MODIS data, in case they are not present locally.
}
\usage{
cloudFill(x, th, year, ModisDir, alpha = FALSE, mask = c(210, 211), filename, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Character. Filename of a raster layer.
}
  \item{th}{
Numeric. The gap percentage threshold [0,1] above which gap filling will be performed
}
  \item{year}{
Numeric. Year of the data
}
  \item{ModisDir}{
Character. Directory where the MODIS VCF data are stored, or will be downloaded to.
}
  \item{alpha}{
Logical or character. If an alpha layer (providing location of the gaps) is to be used, \code{alpha} is the filename of that raster layer.
}
  \item{mask}{
Numeric. Value(s) to be masked.
}
  \item{filename}{
Character. Output filename.
}
  \item{\dots}{
Additional arguments as for \code{\link{writeRaster}}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
pr <- pr$PR[1]
dir = tempdir()
downloadPR(pr, year=2000, dir=dir)
unpackVCF(pr=pr, year=2000, searchDir=dir, dir=sprintf('\%s/\%s',dir,'extract/'))
x <- list.files(sprintf('\%s/\%s',dir,'extract/'))

filename <- sprintf('\%s.tif', rasterTmpFile())
ModisDir <- tempdir()

cloudFill(x=x, th=0.005, ModisDir=ModisDir, filename=filename)

#Visualize the output
r0 <- raster(x)
x[x > 100] <- NA
plot(r0)
plot(r1 <- raster(filename), add=TRUE)



}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
