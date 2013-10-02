\name{WarpModis2Landsat}
\alias{WarpModis2Landsat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reprojects MODIS data using target proj and extent
}
\description{
Creates a character to be used in a system call, or directly in a terminal; and perform reprojection/resampling/subsetting of MODIS data, using proj and extent from a second raster.
}
\usage{
WarpModis2Landsat(target, resolution, ModisInput, ModisSds, filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{target}{
Character. Target raster filename. Full path. Can Also be a list of characters.
}
  \item{resolution}{
Numeric, resolution to resample to.
}
  \item{ModisInput}{
Character. Input modis file.
}
  \item{ModisSds}{
If \code{ModisInput} Numeric. is a hdf file, the subdataset number of the data.
}
  \item{filename}{
Character. Filename of the resulting output file. Full path.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Character string of a gdalwarp expression. Before copy-pasting it in a terminal, you may need to print it using quote=FALSE.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## No example yet.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
