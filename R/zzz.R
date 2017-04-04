#' @useDynLib diffusion, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @import RcppArmadillo
#' @importFrom RcppParallel RcppParallelLibs
NULL

.onUnload <- function(libpath) {
    library.dynam.unload("diffusion", libpath)
}
