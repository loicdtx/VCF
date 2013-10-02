# Author: Loic Dutrieux
# September 2013

cloudFill <- function(x, th, year, ModisDir, alpha=FALSE, mask=c(210, 211), filename, ...) {
    
    #Functions definition
    fileExists <- function(...) { # Similar to file.exists{base}, but accepts a regular expression ... are mostly for path= and pattern=
        list <- list.files(...)
        if (length(list) == 0) return(FALSE)
        if (length(list) > 0) return(TRUE)
    }
    
    # Compare percentage 
    r <- raster(x)
    if (is.character(alpha)) {
        a <- raster(alpha)
        f <- (sum(sapply(X=mask, FUN=function(x) {freq(a, value=x)}))) / ncell(a)
    } else {
        f <- (sum(sapply(X=mask, FUN=function(x) {freq(r, value=x)}))) / ncell(r)
    }    
    if (f <= th) {
        out <- sprintf('Cloud cover (%.3f) below threshold set (%f), no cloud filling performed', f, th)
    } else {
        # Define modis tiles required
        tile <- getTile(r)
        print (sprintf('Area covers %d tile(s), checking whether it (they) already exist locally or not...', length(tile$tile)))
       
        # Check if they already exist locally and download (if necessary)
        
        
        downloader <- function(tile) {
            file <- sprintf('*MOD44B.A%d065.%s.*.hdf', year, tile)
            ex <- fileExists(pattern=glob2rx(file), path=ModisDir, recursive=TRUE)
            if(!ex) {
                getHdf(product='MOD44B', begin=sprintf('%d001', year), end=sprintf('%d365', year), extent=r, collection='005', localArcPath=ModisDir)
            }
            modis <- list.files(pattern=glob2rx(file), path=ModisDir, recursive=TRUE, full.names=TRUE)
            return(modis)
        }
        modisList <- sapply(X=tile$tile, FUN=downloader)
        
        # Warp MOdis2Landsat
        modis30 <- sprintf('%s.tif', rasterTmpFile())
        warpString <- warpModis2Landsat(target=r, ModisInput=modisList, ModisSds=1, filename=modis30) # That function should be able to accept list as ModisInput
        system(warpString)
        
        # Perform values replacements and write directly to file
        modis30 <- raster(modis30)
        if (!is.character(alpha)) {
            overlay(r, modis30, fun=function(x,y) {x[x %in% mask] <- y[x %in% mask]; return(x)}, filename=filename, datatype='INT1U', ...)
        } else {
            overlay(r, modis30, a, fun=function(x,y,z) {x[z %in% mask] <- y[z %in% mask]; return(x)}, filename=filename, datatype='INT1U', ...)
        }
        
        
        out <- sprintf('cloud filling performed successfully for input file %s \n output writen to %s', x, filename)
    }
    return(out)
}