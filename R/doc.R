#' Toy graph to play with diffusion
#'
#' Small graph that can easily be plotted and experimented with.
#' It has graphical parameters included, such as the vertex colour
#' and the layout. It also includes an example input. Has graph attributes
#' with example inputs and outputs, see \code{input_*} and \code{output_*}
#' from \code{list.graph.attributes(graph_toy)}
#'
#' @return An \code{\link[igraph]{igraph}} object
"graph_toy"

#' diffuStats: an R package to compute and benchmark diffusion scores
#'
#' The \code{diffuStats} package consists of (i) functions to compute 
#' graph kernels, see \code{\link[diffuStats]{kernels}}, (ii)
#' the function \code{\link[diffuStats]{diffuse}} to compute the 
#' diffusion scores and (iii) the function 
#' \code{\link[diffuStats]{perf_eval}} and its wrapper 
#' \code{\link[diffuStats]{perf}} to compute performance measures. 
#' The user can find two vignettes in \code{browseVignettes("diffuStats")}: 
#' (1) a quick start with concise examples and (2) 
#' a detailed explanation of the 
#' implemented methods with a practical case study 
#' using a yeast protein dataset.
#'
#' @author Sergio Picart-Armada <sergi.picart@upc.edu>, Alexandre Perera-Lluna
#' @docType package
#' @name diffuStats
NULL
