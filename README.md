# VCF package

R package to facilitate handling (download, cleaning, mosaicking, etc) of the Landsat based tree cover dataset [link](http://landcover.org/data/landsatTreecover/)

## Install the package
```r
library(devtools)
install_github('dutri001/VCF')
```

News:

1.6.0 (2016-10-20)
- Updated valid date range in `unpackVCF` according to newly released VCF products

1.5.0 (2015-06-21)
- `downloadPR` supports download of forest cover change product 
- `downloadPR` has an improved download handler

1.4.0 (2015-01-20)
- `getPR` now supports SP objects as location input
