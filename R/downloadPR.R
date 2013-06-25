# Author: Loic Dutrieux
# June 2013
downloadPR <- function(pr, year, dir, log=NULL, baseURL='ftp://ftp.glcf.umd.edu/glcf/LandsatTreecover/WRS2/') {
  
  if (is.list(pr)) { # Assuming the list provided is the variable returned by getPR() function
    pr <- pr$PR
  }
  
  dir.create(dir, showWarnings = FALSE)
  if(is.null(log)) {
    log = sprintf('%s/downloadVCF.log', dir)
  }
  cat(date(), file=log, sep="\n") #First line of the log file
  
  nbFiles <- length(pr) * length(year)
  print(sprintf('About to start downloading \n %d to download in total', nbFiles))
  
  dl <- function(x, y) { # y is year
    
    # Build URL
    p <- substr(as.character(x),1,3)
    r <- substr(as.character(x),4,6)
    urlP <- sprintf('p%s/r%s/p%sr%s_TC_%d/', p, r, p, r, y) #Path part of the url
    urlF <- sprintf('p%sr%s_TC_%d.tif.gz', p, r, y) # Filename part of the url
    url <- sprintf('%s%s%s', baseURL, urlP, urlF)
    
    
    # Build output string
    filename <- sprintf('%s%s%s', dir, urlP, urlF)
    dir.create(dirname(filename), showWarnings = FALSE, recursive=TRUE)
    
    # Check whether file does already exist or not
    if (!file.exists(filename)) {
      print(sprintf('Downloading %s', url))
      a <- download.file(url=url, destfile=filename)
      if (a == 0) {
        out <- sprintf('%s downloaded successfully', url)
      } else {
        out <- sprintf('%s could not be downloaded', url)
      }      
    } else {
      out <- print(sprintf('File %s already exists, it won\'t be downloaded', basename(filename)))
    }
    return(out)   
  }
  
  
  fun <- function(x, y) {    # Error catching function
    tryReport <- try(dl(x, y))    
    if (inherits(tryReport, 'try-error')) {
      tryReport <- print(sprintf('Something went wrong with pr %d year %d', x, y))
    }
    cat(date(), file=log, sep="\n", append=TRUE)
    cat(tryReport, file=log, sep="\n", append=TRUE)
    return(tryReport)
  }
  
  lapply(X=year, FUN=sapply(X=pr, FUN=fun))  
  
}