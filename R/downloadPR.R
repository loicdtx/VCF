#' Automatic download of vcf data from a list of pathrows
#' 
#' @description Automates the download of Landsat based vcf products (Tree cover and tree cover change) from the ftp server,
#' from a list of list of pathrows. Writes status of the download to a log file
#' and recreates the directory organization of the ftp server locally.
#' 
#' @details Files are downloaded only if they have not been downloaded and written at
#' the same location earlier, the files overwrite previous downloaded files that are == 0 bytes.
#' (Performs some sort of updating of a local archive)
#' 
#' @param pr List or numeric list. Classically the returned object from
#' \code{\link{getPR}}.
#' @param name Character. Product name you wish to download. At the moment \code{'FCC'} and \code{'TC'} can be downloaded.
#' @param year Numeric or list. When \code{name = 'TC'}, a single numeric must be provided. A list of numerics (e.g. \code{c(2000, 2005)} corresponding to the beginning and end of the change interval must ve provided when dowloading Forest Cover Change data).
#' @param dir Character. Directory where to write the downloaded data.
#' @param log character. filename of the logfile. If NULL (default), a file
#' 'downloadVCF.log' is created at the root of \code{dir}
#' @param baseURL character.
#' @param stubborn Numeric. Number of retries when for a reason or another the download fails
#' @return The list of file downloaded, plus eventual warning, or error
#' messages
#' @author Loic Dutrieux
#' @references Land cover: See \url{http://landcover.org/data/landsatTreecover/}
#' Forest cover change: See \url{http://landcover.org/data/landsatFCC/}
#' @keywords Tree cover Landsat, Forest cover change Landsat
#' @examples
#' 
#' \dontrun{
#' pr <- getPR('Belize')
#' pr
#' dir = tempdir()
#' downloadPR(pr, year=2000, name = 'TC', dir=dir)
#' 
#' }
#' 
#' 
#' @export downloadPR

downloadPR <- function(pr, name = 'TC', year, dir, log=NULL, baseURL = 'ftp://ftp.glcf.umd.edu/glcf/', stubborn = 5) {
    
  if(!tolower(name) %in% c('tc', 'fcc')) {
    stop('Product name not supported')
  }
  name <- toupper(name)
 
  if (is.list(pr)) { # Assuming the list provided is the variable returned by getPR() function
    pr <- pr$PR
  }
  pr <- sprintf('%06d', pr) # Make the pr individual objects always 6 digits
  
  dir.create(dir, showWarnings = FALSE, recursive=TRUE)
  if(is.null(log)) {
    log <- sprintf('%sdownloadVCF.log', dir)
  }
  cat(date(), file=log, sep="\n") #First line of the log file
  
  nbFiles <- length(pr)
  print(sprintf('About to start downloading: %d files to download in total', nbFiles))
  
  dl <- function(x, y, name) { # y is year ; x is the pr element and has already been converted to character
    
    # Build URL
    p <- substr(x,1,3)
    r <- substr(x,4,6)
    y <- paste(y, collapse = '')
    if(name == 'FCC') {
      suffix <- '_CM' # Jorn: do you know the difference between CP and CM? Should it be a function argument?
      prefix <- 'LandsatFCC'
    } else {
      suffix <- ''
      prefix <- 'LandsatTreecover'
    }

    urlP <- sprintf('%s/WRS2/p%s/r%s/p%sr%s_%s_%s/', prefix, p, r, p, r, name, y) #Path part of the url
    urlF <- sprintf('p%sr%s_%s_%s%s.tif.gz', p, r, name, y, suffix) # Filename part of the url
    url <- sprintf('%s%s%s', baseURL, urlP, urlF)
   

    
    # Build output string
    filename <- sprintf('%s/%s%s', dir, urlP, urlF)
    dir.create(dirname(filename), showWarnings = FALSE, recursive=TRUE)
    
    
    # Check whether file does already exist or not
    if (file.exists(filename) & !file.info(filename)$size %in% c(0, NA)) { # File exists and has data
      out <- print(sprintf('File %s already exists, it won\'t be downloaded', basename(filename)))      
    } else {
      
      # Prepare download loop
      size <- 0
      count <- 0
      a <- 1
      while(count <= stubborn & (a != 0 | size %in% c(0, NA))) {
        count = count + 1
        print(sprintf('Downloading %s, attempt number %d', url, count))
        a <- try(download.file(url=url, destfile=filename))
        size <- file.info(filename)$size
      }
        
      if (a == 0 & size != 0) {
        out <- sprintf('%s downloaded successfully', url)
      } else {
        out <- sprintf('%s could not be downloaded', url)
      }
    }
    return(out)
  }
 
  
  
  fun <- function(x, y, name) {    # Error catching function
    tryReport <- try(dl(x, y, name))    
    if (inherits(tryReport, 'try-error')) {
      tryReport <- print(sprintf('Something went wrong with pr %s year %d', x, y))
    }
    cat(date(), file=log, sep="\n", append=TRUE)
    cat(tryReport, file=log, sep="\n", append=TRUE)
    return(tryReport)
  }
  
  sapply(X=pr, FUN=fun, y = year, name = name)

  
}

