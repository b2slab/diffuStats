#' Compute graph kernels
#'
#' @description Function \code{commuteTimeKernel}
#' computes the conmute-time kernel,
#' which is the expected time of going back and forth
#' between a couple of nodes
#'
#' @template kernels
#'
#' @rdname kernels
#' @import MASS
#' @import Matrix
#' @import igraph
#' @export
commuteTimeKernel <- function(graph, normalized = FALSE) {
    L <- graph.laplacian(graph = graph, normalized = normalized) %>% as.matrix

    # pseudo-inverse (moore-penrose)
    ans <- MASS::ginv(L)
    rownames(ans) <- colnames(ans) <- V(graph)$name
    ans
}

#' @description Function \code{diffusionKernel}
#' computes the classical diffusion kernel
#' that involves matrix exponentiation. It has a "bandwidth" parameter
#' \eqn{\sigma^2}
#'
# #' @template kernels
#' @rdname kernels
#' @import Matrix
#' @import igraph
#' @export
diffusionKernel <- function(graph, sigma2 = 1, normalized = TRUE) {
    L <- graph.laplacian(graph = graph, normalized = normalized)

    EL <- -sigma2/2*L
    as.matrix(Matrix::expm(EL))
}

#' @description Function \code{inverseCosineKernel}
#' computes the inverse cosine
#' kernel, which is based on a cosine transform on the spectrum of
#' the normalized Laplacian matrix
#'
# #' @template kernels
#' @rdname kernels
#' @import Matrix
#' @import igraph
#' @export
inverseCosineKernel <- function(graph) {
    L <- graph.laplacian(graph = graph, normalized = TRUE) %>% as.matrix

    # need to decompose
    svd.L <- base::svd(L*(pi/4))
    ans <- tcrossprod(svd.L$u %*% diag(cos(svd.L$d)), svd.L$u)
    rownames(ans) <- colnames(ans) <- V(graph)$name
    ans
    # matrixcalc::is.positive.semi.definite(round(kk, 10))
}

#' @description Function \code{pStepKernel}
#' computes the p-step random walk kernel
#'
# #' @template kernels
#'
#' @rdname kernels
#' @importFrom expm %^%
#' @import Matrix
#' @import igraph
#' @export
pStepKernel <- function(graph, a = 2, p = 5L) {
    minusL <- -graph.laplacian(graph = graph, normalized = TRUE)

    # Not optimal but kept for clarity
    # here we restrict to the normalised version, as the eigenvalues are
    # between 0 and 2 -> restriction a >= 2
    stopifnot(a >= 2)
    p <- as.integer(p)
    stopifnot(p > 0)

    Matrix::diag(minusL) <- Matrix::diag(minusL) + a

    if (p == 1L) return(as.matrix(minusL))
    do.call(expm::`%^%`, list(as.matrix(minusL), p))
}

#' @description Function \code{regularisedLaplacianKernel} computes
#' the regularised Laplacian kernel, which is a standard in
#' biological networks. It allows to control the constant terms summed
#' to the diagonal
#'
# #' @template kernels
#' @rdname kernels
#' @import Matrix
#' @import igraph
#' @export
regularisedLaplacianKernel <- function(
    graph,
    sigma2 = 1,
    add_diag = 1,
    normalized = FALSE) {
    L <- graph.laplacian(graph = graph, normalized = normalized)

    RL <- sigma2*L
    Matrix::diag(RL) <- Matrix::diag(RL) + 1

    ans <- as.matrix(Matrix::solve(RL, sparse = FALSE))
    rownames(ans) <- colnames(ans) <- V(graph)$name
    ans
}
