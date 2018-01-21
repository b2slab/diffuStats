#' Compare diffusions to a target score on a grid of parameters
#'
#' Function \code{perf} computes diffusion scores on a grid of
#' parameters and evaluates them using the gold standard scores
#' provided by the user.
#'
#' Function \code{perf} takes a network in
#' \pkg{igraph} format, an initial state
#' to score all the nodes in the network, a target score set.
#' To explore the parameter combinations, it needs a grid and a
#' list of metrics to apply. The validation scores might be only
#' a subset of the network nodes, in which case the metric will
#' be restricted to this set as well.
#'
#'
#' @param scores scores to be smoothed; either a named numeric vector,
#' a column-wise matrix whose rownames are nodes and colnames are
#' different scores, or a named list of such matrices.
#' @param validation target scores to which the smoothed scores
#' will be compared to. Must have the same format as the input scores,
#' although the number of rows may vary and only the matching rows will
#' give a performance measure
#' @param grid_param data frame containing parameter combinations to explore.
#' The column names should be the names of the parameters.
#' @param metric named list of metrics to apply. Each metric should accept
#' the form \code{f(actual, predicted)}
#' @param ... additional named arguments for the diffusion method.
#' It's important
#' to input at least an \code{igraph} object or, alternative, a
#' kernel matrix \code{K}
#'
#' @return A data frame containing the performance of each diffusion score
#'
#' @examples
#' # Using a single vector of scores
#' data(graph_toy)
#' df_perf <- perf(
#'     graph = graph_toy,
#'     scores = graph_toy$input_vec,
#'     validation = graph_toy$input_vec,
#'     grid_param = expand.grid(method = c("raw", "ml")))
#' df_perf
#' # Using a matrix with four set of scores
#' # called Single, Row, Small_sample, Large_sample
#' df_perf <- perf(
#'     graph = graph_toy,
#'     scores = graph_toy$input_mat,
#'     validation = graph_toy$input_mat,
#'     grid_param = expand.grid(method = c("raw", "ml")))
#' df_perf
#'
#' @import plyr
# ' @import magrittr
#' @export
perf <- function(
    scores,
    validation,
    grid_param,
    metric = list(auc = metric_fun(curve = "ROC")),
    ...) {

    # parameter names
    names_param <- colnames(grid_param)

    # function names are needed
    if (is.null(names(metric))) {
        names(metric) <- sapply(metric, function(f) deparse(quote(f)))
    }

    plyr::adply(
        grid_param,
        1,
        function(row_param) {
            # params for do.call
            list_param <- as.list(setNames(row_param, names_param))

            # diffuse
            scores_param <- do.call(
                diffuse,
                c(list(...), list(scores = scores), list_param)
            )

            # data frame with performance
            df_param <- perf_eval(
                prediction = scores_param,
                validation = validation,
                metric = metric)

            # add param column
            data.frame(df_param, list_param)
        }
    )
}
