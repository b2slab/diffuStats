#' Compute performance of diffusion scores on a single case
#'
#' Function \code{perf_eval} directly compares a desired output
#' with the scores from diffusion. It handles the possible
#' shapes of the scores (vector, matrix, list of matrices)
#' and gives the desired metrics.
#'
#'
#' @param prediction smoothed scores; either a named numeric vector,
#' a column-wise matrix whose rownames are nodes and colnames are
#' different scores, or a named list of such matrices.
#' @param validation target scores to which the smoothed scores
#' will be compared to. Must have the same format as the input scores,
#' although the number of rows may vary and only the matching rows will
#' give a performance measure.
#' @param metric named list of metrics to apply. Each metric should accept
#' the form \code{f(actual, predicted)}
#'
#' @return A data frame containing the metrics for each comparable
#' pair of output-validation.
#'
#' @import plyr
#' @import Metrics
# ' @import magrittr
#' @export
perf_eval <- function(
  prediction,
  validation,
  metric = list(auc = Metrics::auc, rmse = Metrics::rmse)) {

  format_pred <- which_format(prediction)
  format_val <- which_format(validation)
  if (format_pred != format_val) {
    stop("'prediction' is ", format_pred,
         " but 'validation' is ", format_val)
  }

  pred <- to_list(prediction)
  val <- to_list(validation)
  # browser()
  ans <- plyr::ldply(
    # iterate over backgrounds
    names(pred),
    function(bkgd) {
      # common names
      common_names <- intersect(
        rownames(val[[bkgd]]),
        rownames(pred[[bkgd]]))
      col_names <- colnames(val[[bkgd]])
      if (any(col_names != colnames(pred)))
        stop("Column names in background ", bkgd, " differ. ",
             "Please make sure they are the same columns.")

      mat_val <- val[[bkgd]][common_names, , drop = FALSE]
      mat_pred <- pred[[bkgd]][common_names, , drop = FALSE]

      # browser()
      # iterate over columns to compute metrics
      df_values <- plyr::ldply(
        col_names,
        function(colname) {
          # iterate over metrics
          df_met <- plyr::llply(
            metric,
            function(metric_fun) {
              do.call(
                metric_fun,
                list(mat_val[, colname], mat_pred[, colname]))
            }
          )
          # metrics data frame with column name
          data.frame(df_met, Column = colname, stringsAsFactors = FALSE)
        }
      )
      # metrics data frame with bkgd and column name
      df_values$Background <- bkgd
      df_values
    }
  )

  # Delete unnecessary columns
  if (format_pred %in% c("vector", "matrix")) ans$Background <- NULL
  if (format_pred == "vector") ans$Column <- NULL

  return(ans)
}
