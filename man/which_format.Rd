% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{which_format}
\alias{which_format}
\title{In which format is the input?}
\usage{
which_format(x)
}
\arguments{
\item{x}{object to evaluate}
}
\value{
character: vector, matrix or list.
}
\description{
Tell apart vector, matrix or list of matrices
}
\examples{
data(graph_toy)
diffuStats:::which_format(graph_toy$input_vec)
diffuStats:::which_format(graph_toy$input_mat)
}
