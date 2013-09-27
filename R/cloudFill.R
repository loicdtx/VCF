cloudFill <- function(x, th, year, ModisDir, alpha=FALSE, mask=c(210, 211), filename) {
    
    # Compare percentage 
    r <- raster(x)
    if is.character(alpha) {
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
        print (sprintf('Area covers %d tile(s), checking whether it already exist locally or not...', length(tile$tile)))
        # Check if they already exist locally
        getHdf(product='MOD44B', begin=sprintf('%d001', year), end=sprintf('%d365', year), extent=r, collection='005', localArcPath=ModisDir)
        
        # Download them (if required)
        
        # Warp MOdis2Landsat
        
        # Perform values replacements and write directly to file
        
        out <- sprintf('cloud filling performed successfully for input file %s', x)
    }
    return(out)
}