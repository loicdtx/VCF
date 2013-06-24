library(maps)
library(mapdata)
library(rgeos)
library(sp)
library(maptools)

# Function to be named getPR

dir = '~/Downloads/'
setwd(dir)    
download.file(url='http://landsat.usgs.gov/documents/wrs2_descending.zip',destfile='wrs2.zip')
unzip(zipfile='wrs2.zip', exdir=sprintf('%swrs2/', dir))
exdir=sprintf('%swrs2/', dir)
setwd(exdir)
over(map('worldHires', 'Brazil'), wrs2)


country <- 'Germany'
extm <- map("world", country, fill=TRUE, col="transparent", plot=FALSE)
IDs <- sapply(strsplit(extm$names, ":"), function(x) x[1])
exts <- map2SpatialPolygons(extm, IDs=IDs, proj4string=CRS("+proj=latlong +datum=WGS84"))

wrs2 <- readShapePoly('wrs2_descending')
proj4string(wrs2) <- '+proj=latlong +datum=WGS84'

x <- wrs2[exts,]




