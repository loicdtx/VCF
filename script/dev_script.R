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

x3 <- lapply(X=x2, FUN=raster)
x3$fun <- mean
x3$filename <- '/media/LP_DUTRIEUX_Data/RS/test/vcf/warp/extract/Belize_3.tif'

do.call(raster::mosaic, x3)

# a <- raster(x2[1])
# b <- raster(x2[2])
# c <- raster(x2[3])
# mosaic(a,b,c, fun=mean, filename='/media/LP_DUTRIEUX_Data/RS/test/vcf/warp/extract/Belize_1.tif')

# cloudFill function
x <- '/media/LP_DUTRIEUX_Data/RS/test/vcf/warp/extract/p019r047_TC_2000.tif'
th <- 0.005
year <- 2000
ModisDir <- '/media/LP_DUTRIEUX_Data/RS/test/vcf/MODIS/'
alpha=FALSE
mask=c(210, 211)
filename <- '/media/LP_DUTRIEUX_Data/RS/test/vcf/warp/extract/p019r047_TC_2000_cloudFill.tif'
cloudFill(x=x,th=th, year=year, ModisDir=ModisDir, alpha=alpha, mask=mask, filename=filename, overwrite=TRUE)




a <- c(1:50)
b <- a*2
a[11:20] <- 100
a[(a == 100) == TRUE] <- b


r <- raster(ncols=36, nrows=18)
r[] <- 1:ncell(r)
s <- raster(ncols=36, nrows=18)
s[] <- 100
s[] <- (1:ncell(s))*2
s[80:300] <- 151
s[300:600] <- 150
mask <- c(150,151)
ss <- s
ss[ss %in% mask] <- r[ss %in% mask]


fun <- function(x,y) {
    x[x %in% mask] <- y[x %in% mask]
    return(x)
}
sss <- overlay(s, r, fun=fun)

?overlay
dr <- getValues(r)
ds <- getValues(s)

dr[1]
fun(ds, dr)

str(dr)

s[s == 150] <- 100


