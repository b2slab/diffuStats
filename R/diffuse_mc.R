# We assume that:
# Scores is a list (background used) of matrices
#   (scores with same background, nodes in rows and sets of scores in columns)
#   Sanity is already checked
#'
#' Compute the heatrank using permutations
#'
#' Function \code{diffuse_mc} has an implemented parallelisation of the
#' Monte Carlo trials for diffusion in a (sparse) network.
#'
#' @param graph igraph object
#' @param scores Recursive list
#' @param n.perm Numeric, number of permutations
#' @param diag.sum What to sum to the Laplacian diagonal?
#' @param sample.prob Numeric, probabilities (needn't be scaled) to permute the
#' input. This is passed to \code{\link[base]{sample}}'s \code{prob} argument.
#' If \code{NULL}, sampling is uniform.
#' @param seed Numeric, seed for random number generator
#' @param oneminusHeatRank Logical, should \code{1 - heatrank}
#' be returned instead of \code{heatrank}?
#' @param K Kernel matrix (if precomputed). If \code{K} is not supplied,
#' the regularised Laplacian will be computed on the fly and used.
#'
#' @return A list containing matrices of heatrank scores
#'
# ' @import Rcpp
#' @importFrom RcppParallel RcppParallelLibs
#' @import RcppArmadillo
#' @import igraph
#' @import Matrix
#' @import plyr
#' @importFrom stats setNames
#' @importFrom methods as
# ' @useDynLib diffusion ParallelHeatrank
#'
#' @export
diffuse_mc <- function(
  graph,
  scores,
  n.perm = 1e4,
  diag.sum = 1,
  sample.prob = NULL,
  seed = 1,
  oneminusHeatRank = TRUE,
  K = NULL)
  # id.latent,
  # p.adjust = "fdr")
{

  if (is.null(K)) {
    message("Matrix not supplied. Computing conductance...")
    L <- graph.laplacian(
      graph = graph,
      normalized = FALSE,
      sparse = TRUE)
    # Connect pathways to boundary
    Matrix::diag(L) <- Matrix::diag(L) + diag.sum

    message("Done")
    message("Computing inverse...")
    K <- as.matrix(Matrix::solve(L))
    rownames(K) <- colnames(K) <- V(graph)$name
    rm(L)
    gc()
    message("Done")
  } else {
    message("Using supplied kernel matrix...")
  }

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

      # bkgd <- as.numeric(names(sample.prob[[scores.name]]))
      # prob <- as.numeric(sample.prob[[scores.name]])
      prob <- (sample.prob[[scores.name]])
      if(!is.null(prob) & (length(prob) != length(bkgd.names)))
        stop(
          "Sampling probabilities have length ",
          length(prob),
          " but the background has, instead, ",
          length(bkgd.names))

      scores.mat <- methods::as(scores[[scores.name]], "sparseMatrix")
      max.sample <- max(Matrix::colSums(scores.mat))

      # Generating permutations...
      message(paste0(scores.name, ": permuting scores..."))

      # library(snow)
      # cl <- snow::makeCluster(c("localhost", "localhost"), type = "SOCK")
      # snow::clusterSetupRNG(cl)
      # snow::clusterExport(cl, c("prob", "max.sample"), envir = environment())
      #
      # perms <- do.call(
      #   "cbind",
      #   snow::clusterApply(
      #     cl,
      #     seq(n.perm),
      #     function(x) {
      #       base::sample(
      #         x = seq_along(prob),
      #         prob = prob,
      #         size = max.sample,
      #         replace = F
      #       )
      #     })
      #   )

      # doParallel::registerDoParallel(cl)
      # parallel::clusterExport(cl, c("prob", "max.sample"))
      message("Permuting...")
      set.seed(seed)
      perms <- t(plyr::laply(
        seq(n.perm),
        function(dummy) {
          sample(
            # x = seq_along(prob),
            x = seq_along(bkgd.names),
            prob = prob,
            size = max.sample,
            replace = FALSE
          )
        },
        .parallel = FALSE))
        # .parallel = F, .progress = "text"))
      # snow::stopCluster(cl)
      gc()

      message(paste0(scores.name, ": computing heatRank..."))
      ans <- ParallelHeatrank(
        K[, bkgd.names],
        perms,
        scores.mat
      )
      # browser()
      rownames(ans) <- rownames(K)
      colnames(ans) <- input.names

      # To check for numeric accuracy....
      # compare to R
      # When tested, the answers were equal (precision 1e-15)
      # ans2 <- apply(
      #   seq(ncol(scores.mat)),
      #   function(col) {
      #     R.subset <- R.whole[, bkgd.names]
      #     T.final <- R.subset %*% scores.mat[, col]
      #     n.in <- sum(scores.mat[, col])
      #     g.null <- as(numeric(nrow(scores.mat)), "sparseMatrix")
      #     scores.null <- plyr::aaply(perms, 2, function(colnull) {
      #       # browser()
      #       g.null[head(colnull, n.in)] <- 1
      #       as.numeric(R.subset %*% g.null)
      #     }, .progress = "text")
      #     heatrank <- rowSums(t(scores.null) > as.numeric(T.final))
      #     heatrank <- (heatrank + 1)/(n.perm + 1)
      #     heatrank
      #   }
      # )

      gc()
      # Make sure we return a matrix
      if (oneminusHeatRank) return(as.matrix(1 - ans))

      return(as.matrix(ans))
    }
  )

  return(ans.all)
}
