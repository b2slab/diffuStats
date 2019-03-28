#' @details Please be aware that the kernel computation can be rather slow and 
#' memory demanding. 
#' This is a reference table of the peak memory usage and computing time
#' for the regularised Laplacian kernel
#' given the order of the network: 
#' 
#' 5k: 900MB & 250s  
#' 
#' 10k: 3,200MB & 2,200s
#' 
#' 15k: 8,000MB & 8,000s
#' 
#' 20k: 13,000MB & 21,000s
#' 
#' However, given a network to study, 
#' this step is a one-time task than can be stored and reused.
#' 
#' @param graph undirected igraph object. 
#' If the edges have weights, those should typically be non-negative.
#' @param sigma2 numeric value, parameter \eqn{\sigma^2} of the kernel - 
#' higher values force more spreading in the network
#' @param add_diag numeric value or vector of length \code{vcount(graph)},
#' term to regularise the spectrum of the Laplacian
#' @param a numeric value greater or equal to 2, which acts as a
#' regularisation term. 
#' Can also be a vector of length \code{vcount(graph)}
#' @param p integer greater than 0, the number of steps for the random walk
#' @param normalized logical, should the normalised (\code{TRUE}) or 
#' unnormalised (\code{FALSE}) graph Laplacian matrix be used?
#'
#' @rdname kernels
#'
#' @return A kernel matrix with adequate dimnames
#'
#' @examples
#' data(graph_toy)
#' K_lap <- regularisedLaplacianKernel(graph_toy)
#' K_diff <- diffusionKernel(graph_toy)
#' K_pstep <- pStepKernel(graph_toy)
#' K_ct <- commuteTimeKernel(graph_toy)
#' K_ic <- inverseCosineKernel(graph_toy)
#' is_kernel(K_lap)
#' 
#' @references 
#' The regularised Laplacian, diffusion, p-step and 
#' inverse cosine kernels: 
#' Smola, A. J., & Kondor, R. (2003, August). 
#' Kernels and regularization on graphs. 
#' In COLT (Vol. 2777, pp. 144-158).
#' 
#' The commute time kernel: 
#' Yen, L., Fouss, F., Decaestecker, C., Francq, P., 
#' & Saerens, M. (2007). Graph nodes clustering based on 
#' the commute-time kernel. 
#' Advances in Knowledge Discovery and Data Mining, 1037-1045.
#' 
#' Benchmark on kernels:
#' Valentini, G., Paccanaro, A., Caniza, H., Romero, A. E., & Re, M. (2014). 
#' An extensive analysis of disease-gene associations 
#' using network integration and fast kernel-based 
#' gene prioritization methods. 
#' Artificial Intelligence in Medicine, 61(2), 63–78. 

