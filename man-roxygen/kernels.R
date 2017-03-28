#' @param graph igraph object
#' @param sigma2 numeric value, parameter \eqn{\sigma^2} of the kernel
#' @param add_diag numeric value or vector equalling \code{vcount(graph)},
#' constant term to regularise the spectrum of the Laplacian
#' @param a numeric value greater or equal to 2, which acts as a
#' regularisation term
#' @param p integer greater than 0, the number of steps for the random walk
#' @param normalized logical, normalize Laplacian matrix?
#'
#' @rdname kernels
#'
#' @return A kernel matrix with adequate dimnames

