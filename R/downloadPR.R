#' Automatic download of vcf data from a list of pathrows
#' 
#' @description Automates the download of Landsat based vcf products from the ftp server,
#' from a list of list of pathrows. Writes status of the download to a log file
#' and recreates the directory organization of the ftp server locally.
#' 
#' @details FIles are downloaded only if they have not been downloaded and written at
#' the same location earlier. (Performs some sort of updating of a local archive)
#' 
#' @param pr List or numeric list. Classically the returned object from
#' \code{\link{getPR}}.
#' @param year Numeric or list (i.e.: c(2000, 2005))
#' @param dir Character. Directory where to write the downloaded data.
#' @param log character. filename of the logfile. If NULL (default), a file
#' 'downloadVCF.log' is created at the root of \code{dir}
#' @param baseURL character.
#' @return The list of file downloaded, plus eventual warning, or error
#' messages
#' @author Loic Dutrieux
#' @references See \url{http://landcover.org/data/landsatTreecover/}
#' @keywords Tree cover Landsat
#' @examples
#' 
#' \dontrun{
#' pr <- getPR('Belize')
#' pr
#' dir = tempdir()
#' downloadPR(pr, year=2000, dir=dir)
#' 
#' }
#' 
#' ftp://ftp.glcf.umd.edu/glcf/LandsatTreecover/WRS2/
#' @export downloadPR

dir <- 'data/'
pr <- getPR('Belize')
year <- 1990

downloadPR <- function(pr, year, dir, log=NULL, baseURL = baseURL) {
    
    if(year == 2000|year == 2005) {
        baseURL <- 'ftp://ftp.glcf.umd.edu/glcf/LandsatTreecover/WRS2/'
    } else if(year == 1990) {
        year <- 19902000
        baseURL <- 'ftp://ftp.glcf.umd.edu/glcf/LandsatFCC/WRS2/'
    } else {
        stop(year, " is not a valid year range")
    }
    
  if (is.list(pr)) { # Assuming the list provided is the variable returned by getPR() function
    pr <- pr$PR
  }
  pr <- sprintf('%06d', pr) # Make the pr individual objects always 6 digits
  
  dir.create(dir, showWarnings = FALSE, recursive=TRUE)
  if(is.null(log)) {
    log <- sprintf('%sdownloadVCF.log', dir)
  }
  cat(date(), file=log, sep="\n") #First line of the log file
  
  nbFiles <- length(pr) * length(year)
  print(sprintf('About to start downloading: %d files to download in total', nbFiles))
  
  dl <- function(x, y) { # y is year ; x is the pr element and has already been converted to character
    
    # Build URL
    p <- substr(x,1,3)
    r <- substr(x,4,6)
    if(year == 2000| year == 2005){
        urlP <- sprintf('p%s/r%s/p%sr%s_TC_%d/', p, r, p, r, y) #Path part of the url
        urlF <- sprintf('p%sr%s_TC_%d.tif.gz', p, r, y) # Filename part of the url
        url <- sprintf('%s%s%s', baseURL, urlP, urlF)
    } else {
        urlP <- sprintf('p%s/r%s/p%sr%s_FCC_%d/', p, r, p, r, y) #Path part of the url
        urlF <- sprintf('p%sr%s_FCC_%d_CM.tif.gz', p, r, y) # Filename part of the url
        url <- sprintf('%s%s%s', baseURL, urlP, urlF)
    }
    
    # Build output string
    filename <- sprintf('%s/%s%s', dir, urlP, urlF)
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
      tryReport <- print(sprintf('Something went wrong with pr %s year %d', x, y))
    }
    cat(date(), file=log, sep="\n", append=TRUE)
    cat(tryReport, file=log, sep="\n", append=TRUE)
    return(tryReport)
  }
  
  for (i in year) {
    sapply(X=pr, FUN=fun, i)
  }
  
}

