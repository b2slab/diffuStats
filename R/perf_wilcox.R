#' Compute column-wise statistics in a performance matrix
#'
#' Function \code{perf_wilcox} compares all the columns of a matrix
#' through a \code{\link[stats]{wilcox.test}}. 
#' The columns are assumed to be performance measures (e.g. AUROC) 
#' whereas the rows are instances. 
#' 
#' The statistical comparison of the columns is intended to 
#' ease comparisons between methods in a rigorous way. 
#' Methods are compared pairwise and a p-value for difference in 
#' performance. 
#' The function \code{perf_wilcox} returns a character matrix so that 
#' (1) the upper triangular matrix contains confidence intervals 
#' on the estimate of the difference between performances, and 
#' (2) the lower triangular matrix contains the two-tailed p-value 
#' that tests difference in performance, with multiple testing correction. 
#' The comparison takes place between row and column in that precise order:
#' a positive difference favours the row and a negative one, the column. 
#'
#' @param perf_mat Numeric matrix whose columns contain performance 
#' metrics of different methods. 
#' @param adjust Function to adjust the p-values for multiple testing. 
#' By default, \code{\link[stats]{p.adjust}} with its default parameters 
#' is used.
#' @param ci Numeric, confidence interval (defaults to \code{0.95})
#' @param digits_ci Integer, digits to display in the confidence interval
#' @param digits_p Integer, digits to display in the p-value
#' @param ... further arguments for \code{\link[base]{format}}
#'
#' @return Character matrix. The upper triangular matrix contains a 
#' confidence interval and the estimate of the pairwise 
#' difference in performance. The lower triangular matrix 
#' shows the associated two-tailed p-value, with multiple testing correction.
#'
#' @examples
#' # Dummy data frame to test 
#' n <- 100
#' perf_mat <- cbind(
#'     good = runif(n = n, min = 0.5, max = 1), 
#'     so_so = runif(n = n, min = 0.2, max = 0.7), 
#'     bad = runif(n = n, min = 0, max = 0.5)
#' )
#' wilcox_mat <- perf_wilcox(perf_mat)
#' 
#' # See how the methods in the rows compare to those
#' # in the columns, confidence interval 
#' # (upper) and p-value (lower)
#' wilcox_mat
#'
#' @importFrom stats p.adjust wilcox.test
#' @export
perf_wilcox <- function(
    perf_mat, 
    adjust = function(p) stats::p.adjust(p, method = "fdr"), 
    ci = 0.95, 
    digits_ci = 2, 
    digits_p = 3, 
    ...) {
    
    # check the input data
    if (length(dim(perf_mat)) != 2)
        stop("'perf_mat' needs to be 2-dimensional")
    if (ncol(perf_mat) < 2)
        stop("'perf_mat' should have at least two columns")
    data_numeric <- "numeric" %in% apply(perf_mat, 2, class)
    if (any(!data_numeric)) 
        stop("Columns in 'perf_mat' must be numeric")
    
    # which methods are being compared?
    method_cols <- colnames(perf_mat)
    n_cols <- length(method_cols)
    
    # declare character matrix
    mat_wilcox <- matrix(
        data = NA_character_, nrow = n_cols, ncol = n_cols, 
        dimnames = list(method_cols, method_cols))
    
    # this gives integer indices, not colnames
    grid_method <- expand.grid(
        seq_along(method_cols), 
        seq_along(method_cols))
    
    # This "for" is not expected to have a large amount of iterations
    # as this function is expected to return human-readable tables
    for (r in seq_len(nrow(grid_method))) {
        # method names
        met1 <- grid_method[r, 1]
        met2 <- grid_method[r, 2]
        
        # test method1-method2 but not the reciprocal
        # method2-method1
        # Therefore, the estimates are row vs column and not the 
        # other way around
        if (met1 < met2) {
            wil_test <- stats::wilcox.test(
                x = perf_mat[, met1], 
                y = perf_mat[, met2], 
                alternative = "two.sided", 
                paired = TRUE, 
                conf.int = TRUE, 
                conf.level = ci
            )
            # confidence interval
            est <- wil_test$estimate
            if (is.na(est)) {
                cell <- NA_character_
            } else {
                cell <- paste0(
                    format(est, digits = digits_ci, ...), 
                    "(", 
                    format(wil_test$conf.int[1], digits = digits_ci, ...), 
                    ",",
                    format(wil_test$conf.int[2], digits = digits_ci, ...), 
                    ")"
                )
            }
            mat_wilcox[met1, met2] <- cell
            
            # pvalue (raw) - needs to be corrected
            p <- wil_test$p.value
            if (is.na(p)) p <- NA_character_ # this switches NaN to NA
            mat_wilcox[met2, met1] <- p
        }
    }
    
    # correction for p-values (default: p.adjust)
    pvals <- do.call(
        adjust, 
        list(as.numeric(mat_wilcox[lower.tri(mat_wilcox)]))
    )

    mat_wilcox[lower.tri(mat_wilcox)] <- format(pvals, digits = digits_p, ...)
    
    mat_wilcox
}
