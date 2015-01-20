#'
#' Reprojects MODIS data using target proj and extent
#' @description Creates a character to be used in a system call, or directly in a terminal; and perform reprojection/resampling/subsetting of MODIS data, using proj and extent from a second raster.
#' @param target Character. Target raster filename. Full path. Can Also be a list of characters.
#' @param resolution Numeric, resolution to resample to.
#' @param ModisInput Character. Input modis file.
#' @param ModisSds Numeric If \code{ModisInput} is a hdf file, the subdataset number of the data.
#' @param filename Character. Filename of the resulting output file. Full path.
#' @return Character string of a gdalwarp expression. Before copy-pasting it in a terminal, you may need to print it using quote=FALSE.
#' @author Loic Dutrieux
#' @import raster


warpModis2Landsat <- function(target, resolution=30, ModisInput, ModisSds, filename) {
    
    
    getProj <- function(raster) {
        if (is.character(raster)) {
            raster <- raster(raster)
        }
        xmin <- raster@extent@xmin
        ymin <- raster@extent@ymin
        xmax <- raster@extent@xmax
        ymax <- raster@extent@ymax
        proj4 <- raster@crs@projargs
        proj44gdal <- sprintf('-t_srs \"%s\"', proj4)
        Extent4gdal <- sprintf('-te %d %d %d %d', xmin, ymin, xmax, ymax)
        return(list(proj4=proj4, proj44gdal=proj44gdal, Extent4gdal=Extent4gdal))
    }
    
    
    if (all(extension(ModisInput) == '.hdf')) {
        fun <- function(x) {
            a <- getSds(x)
            name <- a$SDS4gdal[ModisSds]
            return(name)
        }
        ModisInput <- sapply(X=ModisInput, FUN=fun)
        ModisInput <- gsub("\"", "", ModisInput) # Remove the double quotes
    } 
    ModisInput <- paste(ModisInput, collapse=' ')
    
    
    ProjInfo <- getProj(raster=target)
    te <- ProjInfo$Extent4gdal
    srs <- ProjInfo$proj44gdal
    WarpString <- sprintf('gdalwarp %s %s -r bilinear -tr %d %d \"%s\" %s', te, srs, resolution, resolution, ModisInput, filename)
    return(WarpString)
}
