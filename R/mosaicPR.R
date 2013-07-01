mosaicPR <- function(x, t_srs, res=30, filename) {
  # x is a list of characters (the filenames)
  list <- paste(x, collapse=' ')
  t_srs <- sprintf('\"%s\"', t_srs)
  if (.Platform$OS=='unix') {
    warpstr <- sprintf('gdalwarp -t_srs %s -tr %d %d %s %s', t_srs, res, res, list, filename)
    system(warpstr)
  } 
#   else {
#     warpstr <- sprintf('gdalwarp.exe ')
#     system(warpstr)
#   }
}