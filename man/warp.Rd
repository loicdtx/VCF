\name{warp}
\alias{warp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Warp individual scenes to a given projection
}
\description{
This function allows single scenes to be warped to a given projection. No mosaicking is performed. The function can handle multiple NoData values and can output an alpha layer, which a warped layer containing the NoData values only. 
}
\usage{
warp(x, t_srs, nodata = NULL, filename, res = 30, method = "bilinear", alpha = FALSE, run = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Character. The filename of the layer to be warped.
}
  \item{t_srs}{
character. proj4 expression of the output (\code{filename}) file.
}
  \item{nodata}{
numeric. Value that should not get interpolated in the resampling. Can take multiple values (i.e.: \code{c(220, 210, 211)}). No need to specify the nodata value if that one is included in the file header.
}
  \item{filename}{
character. filename of the output file, with full path.
}
  \item{res}{
numeric. output resolution.
}
  \item{method}{
character. resampling method. See \url{http://www.gdal.org/gdalwarp.html}
}
  \item{alpha}{
Logical. Should an alpha layer be created. Only makes sense in case multiple NoData values are provided.
}
  \item{run}{
logical. should the warping be executed. If set to false, a gdalwarp command string is generated, but not executed.
}
  \item{\dots}{
Extra switches passed to \code{gdalwarp}, see \url{http://www.gdal.org/gdalwarp.html}. }
}
\details{
Requires gdal to be installed on the system, and the the gdal binary folder should be added to the system path. On windows systems, gdal can be install via FWTools or OSGeo4W.
Function behavior is quite different depending on whether single and multiple NoData are provided. \code{gdalwarp}, which is called via a system command does not support multiple NoData values, as a consequence the present function first aggregates all values provided in a single value and does the warping in a second stage.
}
\value{
A character, or list of characters, the gdalwarp command(s). If you inted to copy/past it in a terminal, you can use \code{print()}, with \code{quote=FALSE}.}
\references{
\url{http://www.gdal.org/gdalwarp.html}}
\author{
Loic Dutrieux }
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
dir = tempdir()
downloadPR(pr, year=2000, dir=dir)
unpackVCF(pr=pr, year=2000, searchDir=dir, dir=sprintf('\%s/\%s',dir,'extract/'))
x <- list.files(sprintf('\%s/\%s',dir,'extract/'), full.names=TRUE)
filename <- sprintf('\%s.tif', rasterTmpFile())
warp(x=x[1], t_srs='+proj=laea +lat_0=-10 +lon_0=-70 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs', nodata = c(200, 210, 211, 220), filename=filename)
a <- raster(filename)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ gdal }
\keyword{ Landsat }% __ONLY ONE__ keyword per line
