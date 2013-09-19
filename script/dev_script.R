pr <- getPR('Belize')
dir <- '/media/LP_DUTRIEUX_Data/RS/test/vcf/warp/'
downloadPR(pr, year=2000, dir=dir)
unpackVCF(pr=pr, year=2000, searchDir=dir, dir=sprintf('%s/%s',dir,'extract/'))
x <- list.files(sprintf('%s/%s',dir,'extract/'), full.names=TRUE)


wrap <- function(x) {
  filename <- filename <- sprintf('%s_warp.tif', x)
  w <- warp(x=x, t_srs='+proj=laea +lat_0=-10 +lon_0=-70 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs', nodata = c(200, 210, 211, 220), filename=filename)
  return(w)
}

mclapply(X=x, FUN=wrap, mc.cores=3)

x2 <- list.files('/media/LP_DUTRIEUX_Data/RS/test/vcf/warp/extract/', pattern=glob2rx('*warp*'), full.names=TRUE)

# x3 <- sapply(X=x2, FUN=raster)
# x3$fun <- mean
# x3$filename <- '/media/LP_DUTRIEUX_Data/RS/test/vcf/warp/extract/Belize_1.tif'
# 
# do.call(mosaic, x3)

a <- raster(x2[1])
b <- raster(x2[2])
c <- raster(x2[3])
mosaic(a,b,c, fun=mean, filename='/media/LP_DUTRIEUX_Data/RS/test/vcf/warp/extract/Belize_1.tif')