# Author: Loic Dutrieux
# July 2013





#' Uncompress the .gz archives of the vcf data
#' 
#' Recursively searches through the directories and unpack the queried
#' archives.
#' 
#' %
#' 
#' @param pr List or numeric list. Classically the returned object from
#' \code{\link{getPR}}.
#' @param year Numeric or list (i.e.: c(2000, 2005))
#' @param searchDir character. Root directory of where the archive are stored
#' @param dir character or \code{NULL}, if \code{NULL} (default), data are
#' unpacked in the directories containing the archive. If a directory is
#' provided, all data are unpacked in that directory.
#' @param mc.cores numeric. For parallel implementation, number of workers.
#' @return A list of filenames and potential warning/error messages
#' @note %% ~~further notes~~
#' @section Warning : For parallel implementation, see warning section of
#' \code{\link{mclapply}}
#' @author Loic Dutrieux
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords Landsat .gz
#' @examples
#' 
#' \dontrun{
#' pr <- getPR('Belize')
#' pr
#' dir = tempdir()
#' downloadPR(pr, year=2000, dir=dir)
#' unpackVCF(pr=pr, year=2000, searchDir=dir, dir=sprintf('%s/%s',dir,'extract/'))
#' list.files(sprintf('%s/%s',dir,'extract/'))
#' 
#' }
#' 
#' @export unpackVCF
unpackVCF <- function(pr, year, searchDir, dir=NULL, mc.cores=1) {
  # pr is an object returned by getPR()
  # dir if NULL (defualt), archives are unpacked in the directory containing the archive, else in that specified directory
  # root of where the archive are stored
  # mc.cores = for parallel implementation, number of workers
  
  
  if (is.list(pr)) { # Assuming the list provided is the variable returned by getPR() function
    pr <- pr$PR
  }
  pr <- sprintf('%06d', pr) # Make the pr individual objects always 6 digits
  
  if(!is.null(dir)) {
    dir.create(dir, showWarnings = FALSE, recursive=TRUE)
  }
  
  nbFiles <- length(pr) * length(year)
  print(sprintf('%d files to unpack', nbFiles))
  
  upack <- function(x, y) {
    # Create filename
    p <- substr(x,1,3)
    r <- substr(x,4,6)
    gz <- sprintf('p%sr%s_TC_%d.tif.gz', p, r, y)
    # Search recursively
    file <- list.files(path=searchDir, pattern=gz, full.names=TRUE, recursive=TRUE, include.dirs=FALSE)
    
    # if archive exists, unpack it 
    if (length(file) == 1) { # What happens normally, if only one file is found
      # Unpack
      if (is.null(dir)) {
        R.utils::gunzip(filename=file, remove=FALSE)
      } else {
        destname <- sprintf('%s/%s', dir, substr(gz, 1, nchar(gz) - 3))
        R.utils::gunzip(filename=file, destname=destname, remove=FALSE)
      }
      return(file)
      
    } else { # 0 or more than one file are found
      warning(sprintf('The search for file path/row %s year %s returned %d files, that\'s a problem !!!', x, y, length(file)))
      return(sprintf('Error with file path/row %s year %s, incorrect number of files found', x, y))
    }
    
  }

  fun <- function(x, y) {    # Error catching function
    tryReport <- try(upack(x, y))    
    if (inherits(tryReport, 'try-error')) {
      warning(sprintf('Something went wrong with pr %s year %d', x, y))
    }
    return(tryReport)
  }
  
  for (i in year) {
    out <- mclapply(X=pr, FUN=fun, i, mc.cores=mc.cores)
  }
  return(out)

}
