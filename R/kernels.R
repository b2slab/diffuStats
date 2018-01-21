#' Compute graph kernels
#'
#' @description Function \code{commuteTimeKernel}
#' computes the conmute-time kernel,
#' which is the expected time of going back and forth
#' between a couple of nodes. 
#' If the network is connected, then the commute time kernel
#' will be totally dense, therefore reflecting global properties 
#' of the network.
#' For further details, see [Yen, 2007].
#' This kernel can be computed using both the unnormalised and 
#' normalised graph Laplacian.
#' 
#' @template kernels
#' 
#' @rdname kernels
#' @import MASS
#' @import Matrix
#' @import igraph
#' @export
commuteTimeKernel <- function(graph, normalized = FALSE) {
    if (is.directed(graph)) stop("'graph' must be undirected")
    
    L <- graph.laplacian(graph = graph, normalized = normalized) %>% as.matrix

    # pseudo-inverse (moore-penrose)
    ans <- MASS::ginv(L)
    rownames(ans) <- colnames(ans) <- V(graph)$name
    ans
}

#' @description Function \code{diffusionKernel}
#' computes the classical diffusion kernel
#' that involves matrix exponentiation. It has a "bandwidth" parameter
#' \eqn{\sigma^2} that controls the extent of the spreading.
#' Quoting [Smola, 2003]: 
#' K(x1,x2) can be visualized as the quantity of some substance 
#' that would accumulate at vertex x2 after a given amount of time 
#' if we injected the substance at vertex x1 and let it diffuse 
#' through the graph along the edges.
#' This kernel can be computed using both the unnormalised and 
#' normalised graph Laplacian.
#'
#' @rdname kernels
#' @import Matrix
#' @import igraph
#' @export
diffusionKernel <- function(graph, sigma2 = 1, normalized = TRUE) {
    if (is.directed(graph)) stop("'graph' must be undirected")
    
    L <- graph.laplacian(graph = graph, normalized = normalized)

    EL <- -sigma2/2*L
    as.matrix(Matrix::expm(EL))
}

#' @description Function \code{inverseCosineKernel}
#' computes the inverse cosine
#' kernel, which is based on a cosine transform on the spectrum of
#' the normalized Laplacian matrix.
#' Quoting [Smola, 2003]: the inverse cosine kernel 
#' treats lower complexity functions almost
#' equally, with a significant reduction in the upper end of the spectrum.
#' This kernel is computed using the normalised graph Laplacian.
#'
#' @rdname kernels
#' @import Matrix
#' @import igraph
#' @export
inverseCosineKernel <- function(graph) {
    if (is.directed(graph)) stop("'graph' must be undirected")
    
    L <- graph.laplacian(graph = graph, normalized = TRUE) %>% as.matrix

    # need to decompose
    svd.L <- base::svd(L*(pi/4))
    ans <- tcrossprod(svd.L$u %*% diag(cos(svd.L$d)), svd.L$u)
    rownames(ans) <- colnames(ans) <- V(graph)$name
    ans
}

#' @description Function \code{pStepKernel}
#' computes the p-step random walk kernel. 
#' This kernel is more focused on local properties
#' of the nodes, because random walks are limited in terms 
#' of length. 
#' Therefore, if \code{p} is small, only a fraction of 
#' the values K(x1,x2) will be non-null if the network is sparse
#' [Smola, 2003].
#' The parameter \code{a} is a regularising term that is summed 
#' to the spectrum of the normalised Laplacian matrix, 
#' and has to be \code{2} or greater.
#' The p-step kernels can be cheaper to compute and have been successful 
#' in biological tasks, see the benchmark in [Valentini, 2014].
#'
#' @rdname kernels
#' @importFrom expm %^%
#' @import Matrix
#' @import igraph
#' @export
pStepKernel <- function(graph, a = 2, p = 5L) {
    if (is.directed(graph)) stop("'graph' must be undirected")
    
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
#' biological networks. 
#' The regularised Laplacian kernel arises in numerous situations, 
#' such as the finite difference formulation of the diffusion equation 
#' and in Gaussian process estimation.
#' Sticking to the heat diffusion model, this function 
#' allows to control the constant terms summed
#' to the diagonal through \code{add_diag}, 
#' i.e. the strength of the leaking in each node.
#' If a node has diagonal term of \code{0}, it is not allowed to 
#' disperse heat. 
#' The larger the diagonal term of a node, the stronger the first order 
#' heat dispersion in it, provided that it is positive. 
#' Every connected component in the graph should be able to disperse heat, 
#' i.e. have at least a node \code{i} with \code{add_diag[i] > 0}. 
#' If this is not the case, the result diverges.
#' More details on the parameters can be found in [Smola, 2003]. 
#' This kernel can be computed using both the unnormalised and 
#' normalised graph Laplacian.
#'
#' @rdname kernels
#' @import Matrix
#' @import igraph
#' @export
regularisedLaplacianKernel <- function(
    graph,
    sigma2 = 1,
    add_diag = 1,
    normalized = FALSE) {
    if (is.directed(graph)) stop("'graph' must be undirected")
    
    L <- graph.laplacian(graph = graph, normalized = normalized)

    RL <- sigma2*L
    Matrix::diag(RL) <- Matrix::diag(RL) + add_diag

    ans <- as.matrix(Matrix::solve(RL, sparse = FALSE))
    rownames(ans) <- colnames(ans) <- V(graph)$name
    ans
}
