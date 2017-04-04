#' Create a named list
#'
#' Create a list with variables and name the slots using
#' the variables names
#'
#' @param ... Variables to pack in a list
#'
#' @return A list of variables
#'
#' @examples
#' diffusion:::named.list(LETTERS, mean)
named.list <- function(...) {
    setNames(list(...), as.character(match.call()[-1]))
}

#' Largest connected component
#'
#' Obtain the largest connected component of an \code{igraph} object
#'
#' @param g \code{igraph} object
#'
#' @return A connected \code{igraph} object
#'
#' @examples
#' library(igraph)
#' set.seed(1)
#' g <- erdos.renyi.game(30, p.or.m = .05)
#' largest_cc(g)
#'
#' @import igraph
#' @export
largest_cc <- function(g) {
    cl <- clusters(g)
    cl_max <- which.max(cl$csize)

    igraph::induced_subgraph(graph = g, vids = which(cl$membership == cl_max))
}

#' Translate values into colours
#'
#' Create a vector of hex colours from numeric values,
#' typically diffusion scores
#'
#' @param x numeric vector to be colorised
#' @param range range of values to filter \code{x} (values out of
#' the range will be collapsed to the closest limit)
#' @param n.colors integer, number of colors in the palette
#' @param palette palette function that generates a scale of
#' colours given the number of desired colours. Defaults to a
#' blue-white-red scale by \code{\link[grDevices]{colorRampPalette}}
#'
#' @return Character vector with hex colours
#'
#' @examples
#' set.seed(1)
#' scores2colours(runif(20))
#'
#' @importFrom grDevices colorRampPalette
#' @export
scores2colours <- function(
    x,
    range = c(min(0, min(x)), max(x)),
    n.colors = 10,
    # number 4 and 1 from ggsci::pal_npg()(5)
    palette = colorRampPalette(c("#3C5488FF", "white", "#F39B7FFF"))) {
    pal <- do.call(palette, list(n.colors))

    x[x < range[1]] <- range[1]
    x[x > range[2]] <- range[2]

    y <- (x - range[1])/(range[2] - range[1])*(n.colors - 1)
    z <- round(y) + 1
    pal[z]
}

#' Translate values into shapes
#'
#' Translate 0/1 to shapes, by default \code{"circle"} and
#' \code{"square"}
#'
#' @param x numeric vector to generate shapes from
#' @param shapes character vector with two shapes, respectively
#' zeroes and ones
#'
#' @return Character vector with shapes
#'
#' @examples
#' set.seed(1)
#' scores2shapes(rbinom(n = 20, size = 1, prob = .5))
#'
#' @export
scores2shapes <- function(
    x,
    shapes = c("circle", "square")){

    ifelse(x == 0, shapes[1], shapes[2])
}

#' In which format is the input?
#'
#' Tell apart vector, matrix or list of matrices
#'
#' @param x object to evaluate
#'
#' @return character: vector, matrix or list.
#'
#' @examples
#' data(graph_toy)
#' diffusion:::which_format(graph_toy$input_vec)
#' diffusion:::which_format(graph_toy$input_mat)
which_format <- function(x) {
    if (is.numeric(x) & is.null(dim(x))) return("vector")
    if (is.numeric(x)) return("matrix")
    if (is.list(x)) return("list")

    stop("Non-recognised format, object of class: ", class(x))
}

#' Convert input to list format
#'
#' Convert any input to list format
#'
#' @param scores object to reformat
#' @param dummy_column,dummy_list character, names for the dummy columns/items
#'
#' @return scores in list format
#'
#' @examples
#' data(graph_toy)
#' x_v <- diffusion:::to_list(graph_toy$input_vec)
#' x_m <- diffusion:::to_list(graph_toy$input_mat)
to_list <- function(scores, dummy_column = "X1", dummy_list = "X1") {
    s_format <- which_format(scores)

    if (s_format == "vector") {
        # message("Reshaping score vector to matrix...")

        names_scores <- names(scores)
        scores <- matrix(scores, ncol = 1)
        rownames(scores) <- names_scores
        colnames(scores) <- dummy_column
    }
    if (s_format %in% c("matrix", "vector")) {
        # message("Reshaping score matrix to list...")

        scores <- list(scores)
        names(scores) <- dummy_list
    }
    return(scores)

    stop("Non-recognised format, object of class: ", class(s_format))
}

#' Convert list format to desired format
#'
#' Convert any list format to the convenient one
#'
#' @param scores list to reformat
#' @param x character, desired format
#'
#' @return scores in desired format
#'
#' @examples
#' data(graph_toy)
#' x_v <- diffusion:::to_x_from_list(
#'     diffusion:::to_list(graph_toy$input_vec), "vector")
#' x_m <- diffusion:::to_x_from_list(
#'     diffusion:::to_list(graph_toy$input_vec), "matrix")
to_x_from_list <- function(scores, x) {
    if (x == "list") return(scores)
    if (x == "matrix") return(scores[[1]])
    if (x == "vector")
        return(setNames(scores[[1]][, 1], rownames(scores[[1]])))
}

#' Check if a matrix is a valid kernel
#'
#' This function checks whether the eigenvalues are non-negative
#'
#' @param x numeric, symmetric matrix to be checked
#' @param tol numeric, tolerance for zero eigenvalues
#'
#' @return scores in desired format
#'
#' @examples
#' data(graph_toy)
#' K <- regularisedLaplacianKernel(graph_toy)
#' is_kernel(K)
#' is_kernel(K - 1)
#'
#' @export
is_kernel <- function(x, tol = 1e-8) {
    if (!Matrix::isSymmetric(x))
        stop("the matrix x must be symmetric")
    if (tol <= 0)
        stop("tol must be positive")
    eig_values <- eigen(x, only.values = TRUE)$values
    return(all(eig_values >= -tol))
}
