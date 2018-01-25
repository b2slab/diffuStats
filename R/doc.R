#' Toy graph to play with diffusion
#'
#' Small graph that can easily be plotted and experimented with.
#' It has graphical parameters included, such as the vertex colour
#' and the layout. It also includes an example input. Has graph attributes
#' with example inputs and outputs, see \code{input_*} and \code{output_*}
#' from \code{list.graph.attributes(graph_toy)}
#'
#' @return An \pkg{igraph} object
"graph_toy"

#' diffuStats: an R package to compute and benchmark diffusion scores
#'
#' The \code{diffuStats} package consists of (i) functions to compute 
#' graph kernels, see \code{\link[=kernels]{kernels}}, (ii)
#' the function \code{\link{diffuse}} to compute the 
#' diffusion scores and (iii) the function 
#' \code{\link{perf_eval}} and its wrapper 
#' \code{\link{perf}} to compute performance measures. 
#' The user can find two vignettes in \code{browseVignettes("diffuStats")}: 
#' (1) a quick start with concise examples and (2) 
#' a detailed explanation of the 
#' implemented methods with a practical case study 
#' using a yeast protein dataset.
#'
#' @references 
#' General references:
#' 
#' Most of the graph kernels can be found in:
#' Smola, A. J., & Kondor, R. (2003, August). 
#' Kernels and regularization on graphs. 
#' In COLT (Vol. 2777, pp. 144-158).
#' 
#' The statistical normalisation of the diffusion scores, 
#' which has interest per se, has been introduced in: 
#' Bersanelli, M., Mosca, E., Remondini, D., 
#' Castellani, G., & Milanesi, L. (2016). 
#' Network diffusion-based analysis of high-throughput data 
#' for the detection of differentially enriched modules. 
#' Scientific reports, 6.
#' 
#'
#' @author Sergio Picart-Armada <sergi.picart@upc.edu>, Alexandre Perera-Lluna
#' @docType package
#' @name diffuStats
NULL
