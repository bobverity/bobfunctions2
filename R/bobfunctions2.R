
#------------------------------------------------
# link to Rcpp
#' @useDynLib bobfunctions2, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#------------------------------------------------
# unload dll when package is unloaded
#' @noRd
.onUnload <- function(libpath) {
  library.dynam.unload("bobfunctions2", libpath)
}

