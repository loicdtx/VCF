# Author: Loic Dutrieux
# September 2013

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
