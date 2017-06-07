#' Sanity checks for input
#'
#' .check_scores ensures that scores are suitable for diffusion
#'
#' @param scores scores to check
#'
#' @return Functions return \code{invisible()} but throw
#' warnings and errors as side effect
#' @rdname checks
#'
#' @importFrom stats sd
#' @examples
#' data(graph_toy)
#' diffuStats:::.check_scores(diffuStats:::to_list(graph_toy$input_mat))
.check_scores <- function(scores) {
    form <- which_format(scores)
    scores_names <- names(scores)
    if (is.null(scores_names))
        stop(
            "Scores must be a named list, ",
            " but supplied list contains no names."
        )

    plyr::l_ply(
        scores_names,
        function(mat_name) {
            mat <- scores[[mat_name]]

            # browser()
            if (!is.numeric(mat) & !("dgCMatrix" %in% class(mat))) {
                stop(
                    "The scores in background ",
                    mat_name,
                    " are not numeric!"
                )
            }
            if (any(is.na(mat))) {
                stop(
                    "Scores input cannot contain NA! ",
                    "But background ",
                    mat_name,
                    " does!")
            }
            if (is.null(rownames(mat)))
                stop(
                    "The scores in background ",
                    mat_name,
                    " must have rownames ",
                    "according to the scored nodes!"
                )
            if (is.null(colnames(mat)))
                stop(
                    "The scores in background ",
                    mat_name,
                    " must have colnames ",
                    "to differentiate score sets!"
                )

            std <- apply(mat, 2, stats::sd)
            std_zero <- which(std == 0)
            std_na <- which(is.na(std))


            if (length(std_na))
                warning(
                    "Standard deviation in background ",
                    mat_name,
                    " is NA in columns: ",
                    paste(std_na, collapse = ",")
                )
            if (length(std_zero))
                warning(
                    "Standard deviation in background ",
                    mat_name,
                    " is 0 in columns: ",
                    paste(std_zero, collapse = ",")
                )
        }
    )
    invisible()
}

#' Available methods for diffusion
#'
#' .available_methods is a character vector with the implemented scores
#'
#' @rdname checks
.available_methods <- c("raw", "ml", "gm", "mc", "z", "ber_s", "ber_p")

#' Check method sanity
#'
#' .check_method ensures that 'method' is a valid character
#'
#' @param method object to test
#'
#' @rdname checks
#'
#' @examples
#' diffuStats:::.check_method("raw")
.check_method <- function(method) {
    if (!is.character(method) & !is.factor(method))
        stop(
            "The supplied 'method' must be a character, ",
            "but the one supplied is a ",
            class(method))

    if (length(method) > 1)
        stop(
            "Only one 'method' can be supplied at once, ",
            "but you supplied ",
            length(method))


    if (!(method %in% .available_methods))
        stop(
            "The available methods are ",
            paste(.available_methods, collapse = ","),
            " but you supplied '",
            method,
            "', which is not implemented")

    invisible()
}

#' Check metric sanity
#'
#' .check_metric ensures that 'metric' is a valid list of metric functions
#'
#' @param metric object to test
#'
#' @rdname checks
#'
#' @examples
#' diffuStats:::.check_metric(list(auc = Metrics::auc))
.check_metric <- function(metric) {
    if (!is.list(metric))
        stop(
            "'metric' must be a list of metrics, ",
            "but it is not a list")
    names_metric <- names(metric)
    if (is.null(names_metric))
        stop(
            "'metric' must be a named list, but supplied list ",
            "has no names.")

    plyr::l_ply(
        names_metric,
        function(met) {
            # browser()
            fun_met <- (metric[[met]])
            if (!is.function(fun_met))
                stop(
                    "In the 'metric' list supplied, the metric named",
                    met,
                    " is not actually a function, but a ",
                    paste(class(fun_met), collapse = ","))
            fun_formals <- formals(fun_met)
            if (length(fun_formals) < 2)
                stop(
                    "In the 'metric' list supplied, the metric named",
                    met,
                    " has less than 2 arguments: ",
                    paste(names(fun_formals), collapse = ",")
                )
            if (length(fun_formals) > 2)
                warning(
                    "In the 'metric' list supplied, the metric named",
                    met,
                    " has more than 2 arguments: ",
                    paste(names(fun_formals), collapse = ","),
                    ". Make sure this is a proper metric function."
                )
            dummy_call <- do.call(
                fun_met,
                list(c(1, 0, 0), c(.75, .25, .2)))
            if (length(dummy_call) > 1)
                warning(
                    "A sanity check call of the 'metric' function ",
                    met,
                    " returns a vector with length greater than 1. ",
                    "Is the metric correct?"
                )
        }
    )
    invisible()
}

#' Check graph sanity
#'
#' .check_graph ensures that 'graph' is a valid \code{igraph} object
#'
#' @param graph object to test
#'
#' @rdname checks
#'
#' @examples
#' data(graph_toy)
#' diffuStats:::.check_graph(graph_toy)
.check_graph <- function(graph) {
    if (missing(graph) | is.null(graph)) return(invisible())
    if (!is.igraph(graph))
        stop("'graph' must be an igraph object")

    node_name <- V(graph)$name
    if (is.null(node_name))
        stop(
            "'graph' must have node names! ",
            "Set them through V(graph)$name <- ")
    if (any(is.na(node_name)))
        stop(
            "'graph' cannot have NA as node names")
    if (any(duplicated(V(graph)$name)))
        stop(
            "'graph' has non-unique names! ",
            "Please check that the names are unique."
        )
    if (is.directed(graph))
        warning(
            "'graph' should be an undirected igraph object. ",
            "You should use as.undirected"
        )
    edge_weight <- E(graph)$weight # can be null
    if (!is.null(edge_weight)) {
        if (any(is.na(edge_weight)))
            stop("'graph' cannot contain NA edge weights")
        if (any(edge_weight < 0))
            warning("'graph' should not contain negative edge weights")
    }

    invisible()
}


#' Check kernel sanity
#'
#' .check_K ensures that 'K' is a formally valid kernel.
#' Does not check for spd
#'
#' @param K object to test
#'
#' @rdname checks
#'
#' @examples
#' data(graph_toy)
#' diffuStats:::.check_K(regularisedLaplacianKernel(graph_toy))
.check_K <- function(K) {
    if (!is.matrix(K))
        stop("'K' must be a matrix")

    if (!is.numeric(K))
        stop("'K' must be a numeric matrix, but it is not numeric.")
    if (any(is.na(K)))
        stop("'K' cannot have NA as values")

    if (nrow(K) != ncol(K))
        stop(
            "'K' must be a square matrix, but it has ",
            nrow(K),
            " rows and ",
            ncol(K),
            " columns.")

    rown <- rownames(K)
    coln <- colnames(K)

    if (is.null(rown))
        stop("'K' kernel must have rownames!")
    if (is.null(coln))
        stop("'K' kernel must have colnames!")
    if (any(is.na(c(rown, coln))))
        stop("'K' dimnames cannot be NA")
    if (any(rown != coln))
        stop("'K' rownames and colnames must coincide")
    if (any(duplicated(rown)))
        stop("'K' cannot contain duplicated rownames/colnames")

    invisible()
}
