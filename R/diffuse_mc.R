#' Compute the heatrank using permutations
#'
#' Function \code{diffuse_mc} has an implemented parallelisation of the
#' Monte Carlo trials for diffusion in a network.
#' The input scores are assumed to be sparse and are 
#' internally sparsified, so very dense scores 
#' migth take time with current implementation.
#'
#' @param graph igraph object
#' @param scores Recursive list, can have either binary or quantitative
#' scores
#' @param n.perm Numeric, number of permutations
#' @param sample.prob Numeric, probabilities (needn't be scaled) to permute the
#' input. This is passed to \code{\link[base]{sample}}'s \code{prob} argument.
#' If \code{NULL}, sampling is uniform. It has to be in a list format,
#' with the same names as \code{scores}, and each element of the list
#' must be the sampling probability of each background.
#' @param seed Numeric, seed for random number generator
#' @param oneminusHeatRank Logical, should \code{1 - heatrank}
#' be returned instead of \code{heatrank}?
#' @param K Kernel matrix (if precomputed). If \code{K} is not supplied,
#' the regularised Laplacian will be computed on the fly and used.
#' @param ... currently ignored arguments
#'
#' @return A list containing matrices of heatrank scores
#'
#' @importFrom RcppParallel RcppParallelLibs
#' @import RcppArmadillo
#' @import igraph
#' @import Matrix
#' @import plyr
#' @importFrom stats setNames
#' @importFrom methods as
#'
#' @examples
#' # Using a list as input (needed)
#' data(graph_toy)
#' list_input <- list(myInput1 = graph_toy$input_mat)
#' diff_mc <- diffuse_mc(
#'     graph = graph_toy,
#'     scores = list_input)
#'
#' @export
diffuse_mc <- function(
    graph,
    scores,
    n.perm = 1e4,
    sample.prob = NULL,
    seed = 1,
    oneminusHeatRank = TRUE,
    K = NULL,
    ...)
{
    # sanity checks
    .check_scores(scores)

    # Kernel matrix
    if (is.null(K)) {
        .check_graph(graph)
        message(
            "Kernel not supplied. ",
            "Computing regularised Laplacian kernel ...")
        K <- regularisedLaplacianKernel(graph = graph)
        message("Done")
    } else {
        .check_K(K)
        message("Using supplied kernel matrix...")
    }

    # Iterate over all scores backgrounds
    ans.all <- plyr::llply(
        stats::setNames(names(scores), names(scores)),
        function(scores.name) {
            # match indices (NO NAMES, careful)
            bkgd.names <- rownames(scores[[scores.name]])
            input.names <- colnames(scores[[scores.name]])

            if (!all(bkgd.names %in% rownames(K)))
                stop(
                    "In background ",
                    scores.name,
                    ", some of the input node names ",
                    "are not found in the kernel! ",
                    "Check that the rownames of the input are ",
                    "contained in the names of the graph nodes."
                )
            
            prob <- (sample.prob[[scores.name]])
            if(!is.null(prob) & (length(prob) != length(bkgd.names)))
                stop(
                    "Sampling probabilities have length ",
                    length(prob),
                    " but the background has, instead, ",
                    length(bkgd.names))

            scores.mat <- methods::as(scores[[scores.name]], "sparseMatrix")
            # maximum number of non-zero entries
            max.sample <- max(Matrix::colSums(scores.mat != 0))

            # Generating permutations...
            message(paste0(scores.name, ": permuting scores..."))

            # Generate permutations with R
            message("Permuting...")
            set.seed(seed)
            perms <- t(plyr::laply(
                seq(n.perm),
                function(dummy) {
                    sample(
                        x = seq_along(bkgd.names),
                        prob = prob,
                        size = max.sample,
                        replace = FALSE
                    )
                },
                .parallel = FALSE))

            # .. and compute scores using c++ code
            message(paste0(scores.name, ": computing heatRank..."))
            ans <- ParallelHeatrank(
                K[, bkgd.names],
                perms,
                scores.mat
            )
            rownames(ans) <- rownames(K)
            colnames(ans) <- input.names
            
            # Make sure we return a matrix
            if (oneminusHeatRank) return(as.matrix(1 - ans))

            return(as.matrix(ans))
        }
    )

    return(ans.all)
}
