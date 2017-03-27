#' Diffuse scores on a network
#'
#' Function \code{diffuse} takes a network in
#' \code{\link[igraph]{igraph}} format and an initial state
#' to score all the nodes in the network.
#'
#' Input scores can be specified at three complexity levels.
#' A single set of scores to smooth can be represented as a named
#' numeric vector, whereas if several of these vectors that share
#' the node names need to be smoothed, they can be provided as
#' a column-wise matrix. However, if the number of unlabelled
#' entities varies from one case to another, a named list of such scores
#' matrices can be passed to this function. The input format will
#' be kept in the output.
#'
#' @param graph \code{\link[igraph]{igraph}} object for the diffusion
#' @param scores scores to be smoothed; either a named numeric vector,
#' a column-wise matrix whose rownames are nodes and colnames are
#' different scores, or a named list of such matrices.
#' @param method character, one of \code{raw}, \code{gm},
#' \code{ml}, \code{z}, \code{mc}, \code{ber_s}, \code{ber_p}
#' @param ... additional arguments for the diffusion method
#'
#' @return A list of scores, with the same length and
#' dimensions as \code{scores}
#'
#' @import igraph
# ' @import magrittr
#' @export
diffuse <- function(
  graph,
  scores,
  method,
  ...) {

  # classical <- c("raw", "ml", "gm")
  #
  # For data reshaping
  dummy_column <- "dummy"
  dummy_list <- "dummy"

  # Format scores
  is_vector <- is.numeric(scores) & is.null(dim(scores))
  is_numeric <- is.numeric(scores)

  if (is_vector) {
    message("Reshaping score vector to matrix...")

    names_scores <- names(scores)
    scores <- matrix(scores, ncol = 1)
    rownames(scores) <- names_scores
    colnames(scores) <- dummy_column
  }
  if (is_numeric) {
    message("Reshaping score matrix to list...")

    scores <- list(scores)
    names(scores) <- dummy_list
  }

  if (method == "raw") {
    ans <- (diffuse_raw(graph = graph, scores = scores, ...))
  }
  if (method == "ml") {
    scores_ml <- lapply(
      scores,
      function(mat) {
        mat[mat == 0] <- -1
        mat
      }
    )
    # browser()
    ans <- (diffuse_raw(graph = graph, scores = scores_ml, ...))
  }
  if (method == "gm") {
    scores_gm <- lapply(
      scores,
      function(mat) {
        # browser()
        ids_nobkgd <- setdiff(V(graph)$name, rownames(mat))
        n_tot <- vcount(graph)
        n_bkgd <- nrow(mat)

        # normalisation has to be performed for each column, as it depends
        # on the number of positives and negatives...
        mat_complete <- apply(
          mat,
          2,
          function(col) {
            n_pos <- sum(col)
            n_neg <- n_bkgd - n_pos

            col[col == 0] <- -1

            p <- (n_pos - n_neg)/(n_tot)
            c(col, rep(p, n_tot - n_bkgd))
          }
        )
        rownames(mat_complete) <- c(rownames(mat), ids_nobkgd)

        # sort the names as in the original graph
        mat_complete[V(graph)$name, , drop = FALSE]
      }
    )
    ans <- (diffuse_raw(graph = graph, scores = scores_gm, ...))
  }

  # Monte-Carlo simulations
  if (method == "mc") {
    ans <- (diffuse_mc(graph = graph, scores = scores, ...))
  }

  # z scores
  if (method == "z") {
    ans <- (diffuse_raw(graph = graph, scores = scores, z = TRUE, ...))
  }

  # Bersanelli's scores (s)
  if (method == "ber_s") {
    list_dots <- list(...)

    if ("eps" %in% names(list_dots)) {
      eps <- list_dots$eps
    } else {
      eps <- 1
    }
    # Compute final state
    scores_raw <- diffuse_raw(graph = graph, scores = scores, ...)

    scores_ber_s <- plyr::llply(
      setNames(names(scores), names(scores)),
      function(scores_name) {
        # each list can have a different background...
        # nodes outside the background will be assigned a prior score of 0
        mat_out <- scores_raw[[scores_name]]
        mat_in <- scores[[scores_name]]

        # matrix with correct dimnames but populated with eps
        mat_in_fill <- Matrix::Matrix(
          data = eps,
          nrow = nrow(mat_out),
          ncol = ncol(mat_out),
          dimnames = dimnames(mat_out))

        # add the original input: only in the rows that match
        mat_in_fill[rownames(mat_in), ] <-
          mat_in_fill[rownames(mat_in), ] + mat_in

        mat_out/mat_in_fill
      }
    )
    ans <- (scores_ber_s)
  }

  # Bersanelli's scores (p)
  if (method == "ber_p") {
    # Compute final state
    scores_raw <- diffuse_raw(graph = graph, scores = scores, ...)

    # Compute p
    scores_mc <- diffuse_mc(
      graph = graph,
      scores = scores,
      oneminusHeatRank = FALSE,
      ...)

    scores_ber_p <- plyr::llply(
      setNames(names(scores), names(scores)),
      function(scores_name) {
        s_mc <- scores_mc[[scores_name]]
        s_raw <- scores_raw[[scores_name]]

        -log10(s_mc)*s_raw
      }
    )
    ans <- (scores_ber_p)
  }
  if (!exists("ans")) {
    message(
      "The specified method ",
      method,
      " is not implemented and will return NULL")
    return(invisible())
  }

  # reshape back to original
  if (is_numeric) {
    # message("Reshaping results list to matrix...")

    ans <- ans[[dummy_list]]
  }
  if (is_vector) {
    # message("Reshaping results matrix to vector...")

    ans <- setNames(as.vector(ans[, dummy_column]), rownames(ans))
  }

  message("All done")
  return(ans)

}
