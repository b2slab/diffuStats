#' Create a named list
#'
#' Create a list with variables and name the slots using
#' the variables names
#'
#' @param ... Variables to pack in a list
#'
#' @return A list of variables
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
#' @export
scores2shapes <- function(
  x,
  shapes = c("circle", "square")){

  ifelse(x == 0, shapes[1], shapes[2])
}

