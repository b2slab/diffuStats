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
#' diffuStats:::named.list(LETTERS, mean)
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
#' blue-white-red scale by \code{\link[grDevices:colorRamp]{colorRampPalette}}
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
    palette = colorRampPalette(c("#3C5488FF", "white", "#F39B7FFF"))) {
    pal <- do.call(palette, list(n.colors))

    # colors in default argument come from 
    # number 4 and 1 from ggsci::pal_npg()(5)
    
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
#' diffuStats:::which_format(graph_toy$input_vec)
#' diffuStats:::which_format(graph_toy$input_mat)
which_format <- function(x) {
    if (is.numeric(x) & is.null(dim(x))) return("vector")
    if (is.numeric(x)) return("matrix")
    if (is.list(x)) return("list")

    stop("Non-recognised input scores format, object of class: ", class(x))
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
#' x_v <- diffuStats:::to_list(graph_toy$input_vec)
#' x_m <- diffuStats:::to_list(graph_toy$input_mat)
to_list <- function(scores, dummy_column = "X1", dummy_list = "X1") {
    s_format <- which_format(scores)

    if (s_format == "vector") {
        names_scores <- names(scores)
        scores <- matrix(scores, ncol = 1)
        rownames(scores) <- names_scores
        colnames(scores) <- dummy_column
    }
    if (s_format %in% c("matrix", "vector")) {
        scores <- list(scores)
        names(scores) <- dummy_list
    }
    return(scores)

    stop(
        "Non-recognised input scores format, object of class: ",
        class(s_format))
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
#' x_v <- diffuStats:::to_x_from_list(
#'     diffuStats:::to_list(graph_toy$input_vec), "vector")
#' x_m <- diffuStats:::to_x_from_list(
#'     diffuStats:::to_list(graph_toy$input_vec), "matrix")
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

#' Compute the area under the curves (ROC, PRC)
#'
#' Function \code{metric_auc} computes the AUROC 
#' (Area Under the Receiver Operating Characteristic Curve) 
#' and the AUPRC 
#' (Area Under the Precision Recall Curve), measures of goodness 
#' of a ranking in a binary classification problem. 
#' Partial areas are also supported.
#' Important: the higher ranked classes are assumed to 
#' ideally target positives (label = \code{1}) whereas 
#' lower ranks correspond to negatives (label = \code{0}).
#' 
#' The AUROC is a scalar value: the probability of a 
#' randomly chosen positive having a higher rank than a randomly 
#' chosen negative. 
#' AUROC is cutoff-free and an informative of the performance of a 
#' ranker. Likewise, AUPRC is the area under the Precision-Recall curve 
#' and is also a standard metric for binary classification. 
#' Both measures can be found in [Saito, 2017].
#' 
#' AUROC and AUPRC have their partial counterparts, in which 
#' only the area enclosed up to a certain false positive rate (AUROC) 
#' or recall (AUPRC) is accounted for. 
#' This can be useful when assessing the goodness of the ranking, 
#' focused on the top entities.
#' 
#' The user can, however, define his or her custom performance 
#' metric. AUROC and AUPRC are common choices, but other 
#' problem-specific metrics might be of interest. 
#' For example, number of hits in the top k nodes. 
#' Machine learning metrics can be found in packages such as 
#' \code{Metrics} and \code{MLmetrics} from the CRAN repository
#' (\url{http://cran.r-project.org/}).
#'
#' @param actual numeric, binary labels of the negatives (\code{0}) 
#' and positives (\code{1})
#' @param predicted numeric, prediction used to rank the entities - 
#' this will typically be the diffusion scores
#' @param curve character, either \code{"ROC"} for computing the 
#' AUROC or \code{"PRC"} for the AUPRC
#' @param partial vector with two numeric values for computing partial 
#' areas. The numeric values are the limits in the \code{x} axis 
#' of the curve, as implemented in the \code{"xlim"} argument 
#' in \code{\link[precrec]{part}}. Defaults to \code{c(0,1)}, i.e. the 
#' whole area
#' @param standardized logical, should partial areas be standardised
#' to range in [0, 1]? Defaults to \code{FALSE} and only affects 
#' partial areas.
#' 
#' @return \code{metric_auc} returns a numeric value, the 
#' area under the specified curve
#'
#' @examples
#' # generate class and numeric ranking
#' set.seed(1)
#' n <- 50
#' actual <- rep(0:1, each = n/2)
#' predicted <- ifelse(
#'     actual == 1, 
#'     runif(n, min = 0.2, max = 1), 
#'     runif(n, min = 0, max = 0.8))
#' 
#' # AUROC
#' metric_auc(actual, predicted, curve = "ROC")
#' 
#' # partial AUC (up until false positive rate of 10%)
#' metric_auc(
#'     actual, predicted, curve = "ROC", 
#'     partial = c(0, 0.1))
#' 
#' # The same are, but standardised in (0, 1)
#' metric_auc(
#'     actual, predicted, curve = "ROC", 
#'     partial = c(0, 0.1), standardized = TRUE)
#' 
#' # AUPRC
#' metric_auc(actual, predicted, curve = "PRC")
#'
#' # Generate performance functions for perf and perf_eval
#' f_roc <- metric_fun(
#'     curve = "ROC", partial = c(0, 0.5), 
#'     standardized = TRUE)
#' f_roc
#' f_roc(actual = actual, predicted = predicted)
#'
#' @references Saito, T., & Rehmsmeier, M. (2017). 
#' Precrec: fast and accurate precision–recall 
#' and ROC curve calculations in R. 
#' Bioinformatics, 33(1), 145-147.
#' 
#' @importFrom precrec evalmod part pauc 
#' 
#' @export
metric_auc <- function(
    actual, 
    predicted, 
    curve = "ROC", 
    partial = c(0, 1), 
    standardized = FALSE) {
    # compute "prediction" object
    # browser()
    pred <- try({
        mod <- precrec::evalmod(
            scores = predicted, labels = actual)
    })
    
    # return NA if the area cannot be computed
    if (inherits(pred, "try-error")) return(NA)
    
    # compute the requested areas
    areas <- precrec::part(mod, xlim = partial)
    
    # as a data frame
    ans_df <- precrec::pauc(areas)
    ans_df <- ans_df[ans_df$curvetypes == curve, ]

    # return desired metric
    if (standardized) {
        return(ans_df$spaucs)
    } else {
        return(ans_df$paucs)
    }
}

#' Helper function to build a metric evaluation function
#' 
#' Function \code{metric_fun} is a wrapper on \code{metric_auc} that 
#' returns a function for performance evaluation. This function takes 
#' as input actual and predicted values and outputs a performance metric. 
#' This is needed for functions such as \code{\link[diffuStats]{perf}} 
#' and \code{\link[diffuStats]{perf_eval}}, which iterate over a 
#' list of such metric functions and return the performance 
#' measured through each of them.
#' 
#' @param ... parameters to pass to \code{\link[diffuStats]{metric_auc}}
#' 
#' @return \code{metric_fun} returns a function (performance metric) 
#' 
#' @rdname metric_auc
#' @export
metric_fun <- function(...) {
    function(actual, predicted) {
        metric_auc(actual = actual, predicted = predicted, ...)
    }
}
