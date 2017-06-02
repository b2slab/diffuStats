#' Diffuse scores on a network
#'
#' Function \code{diffuse} takes a network in
#' \code{\link[igraph]{igraph}} format and an initial state
#' to score all the nodes in the network.
#'
#' @param graph \code{\link[igraph]{igraph}} object for the diffusion
#' @param scores list of score matrices. For a single input with a
#' single background, supply a list with a vector column
#' @param z logical, should z-scores be computed instead of raw scores?
#' @param K optional matrix, precomputed diffusion kernel
#' @param ... currently ignored arguments
#'
#' @return A list of scores, with the same length and
#' dimensions as \code{scores}
#'
#' @examples
#' # Using a list as input (needed)
#' data(graph_toy)
#' list_input <- list(myInput1 = graph_toy$input_mat)
#' diff_raw <- diffuse_raw(
#'     graph = graph_toy,
#'     scores = list_input)
#' diff_z <- diffuse_raw(
#'     graph = graph_toy,
#'     scores = list_input,
#'     z = TRUE)
#' @import igraph
#' @export
diffuse_raw <- function(
    graph,
    scores,
    z = FALSE,
    K = NULL,
    ...) {
    # sanity checks
    # if (!missing(graph))
    # if (!is.null(K))
    .check_scores(scores)

    # Kernel matrix
    if (is.null(K)) {
        .check_graph(graph)
        message(
            "Kernel not supplied. ",
            "Computing regularised Laplacian kernel ...")
        K <- regularisedLaplacianKernel(graph = graph)
        gc()
        message("Done")
    } else {
        .check_K(K)
        message("Using supplied kernel matrix...")
    }

    # Compute scores
    # browser()
    ans.all <- plyr::llply(
        stats::setNames(names(scores), names(scores)),
        function(scores.name) {
            # n <- nrow(R.whole)
            # scores.name <- "bkgd1"
            # browser()
            # match indices (NO NAMES, careful)
            bkgd.names <- rownames(scores[[scores.name]])
            input.names <- colnames(scores[[scores.name]])

            # input scores
            scores.mat <- methods::as(scores[[scores.name]], "sparseMatrix")

            # raw scores
            diff.raw <- K[, bkgd.names] %*% scores.mat

            # browser()
            rownames(diff.raw) <- rownames(K)
            colnames(diff.raw) <- input.names

            gc()
            # Return base matrix if it is raw
            # Continue if we want z-scores
            if (z == FALSE) return(as.matrix(diff.raw))

            # If we want z-scores, must compute rowmeans and rowmeans2
            .rowSums <- Matrix::rowSums(K[, bkgd.names])
            .rowSums2 <- Matrix::rowSums(K[, bkgd.names] ** 2)

            # Constant terms over columns
            n <- length(bkgd.names)
            const_mean <- .rowSums/n
            const_var <-  (n*.rowSums2 - .rowSums**2)/((n - 1)*(n**2))

            # diff.z <- apply(
            #   diff.raw,
            #   2,
            #   function(col) {
            #     s1 <- sum(col)
            #     s2 <- sum(col**2)
            #
            #     # means and vars depend on first and second moments
            #     # of the input. This should be valid for non-binary
            #     # inputs as well
            #     score_means <- const_mean*s1
            #     score_vars <- const_var*(n*s2 - s1**2)
            #
            #     (col - score_means)/sqrt(score_vars)
            #   }
            # )
            diff.z <- sapply(
                1:ncol(diff.raw),
                function(col_ind) {
                    col_in <- scores.mat[, col_ind]
                    col_raw <- diff.raw[, col_ind]

                    s1 <- sum(col_in)
                    s2 <- sum(col_in**2)

                    # means and vars depend on first and second moments
                    # of the input. This should be valid for non-binary
                    # inputs as well
                    score_means <- const_mean*s1
                    score_vars <- const_var*(n*s2 - s1**2)

                    (col_raw - score_means)/sqrt(score_vars)
                }
            )
            # Give correct names
            dimnames(diff.z) <- dimnames(diff.raw)

            # browser()
            # if (any(is.na(diff.z))) browser()
            #
            # This is already a base matrix
            return(diff.z)
        }
    )

    return(ans.all)

}
