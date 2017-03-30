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
#' @examples
#' # Using a vector
#' data(graph_toy)
#' diff_scores <- diffuse(
#'   graph = graph_toy,
#'   scores = graph_toy$input_vec,
#'   method = "raw")
#'
#' # Using a matrix
#' data(graph_toy)
#' diff_scores <- diffuse(
#'   graph = graph_toy,
#'   scores = graph_toy$input_mat,
#'   method = "raw")
#'
#' # Using a list of matrices
#' data(graph_toy)
#' diff_scores <- diffuse(
#'   graph = graph_toy,
#'   scores = list(myScores1 = graph_toy$input_mat,
#'                 myScores2 = graph_toy$input_mat),
#'   method = "raw")
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
  format_scores <- which_format(scores)
  scores <- to_list(scores)

  # Check if we have a graph or a kernel
  if (!missing("graph")) {
    format_network <- "graph"
  } else {
    if (!("K" %in% names(list(...))))
      stop("Neither a graph 'graph' or a kernel 'K' were provided")
    format_network <- "kernel"
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
        # Have to match rownames with background
        # If the kernel is provided...
        if (format_network == "graph") {
          names_ordered <- V(graph)$name
        } else if (format_network == "kernel") {
          names_ordered <- rownames(list(...)[["K"]])
        }
        #
        # If the graph is defined...
        ids_nobkgd <- setdiff(names_ordered, rownames(mat))
        n_tot <- length(names_ordered)
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
        mat_complete[names_ordered, , drop = FALSE]
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

        as.matrix(mat_out/mat_in_fill)
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

  message("All done")
  # reshape back to original and return
  return(to_x_from_list(ans, format_scores))
}
