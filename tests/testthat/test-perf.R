context("Grid search for performance evaluation")

data("graph_toy")
set.seed(1)

test_that("Grid search works", {
  expect_error({
    df_perf <- perf(
      graph = graph_toy,
      scores = graph_toy$input_mat,
      validation = graph_toy$input_mat,
      grid_param = expand.grid(
        method = c("raw", "z", "mc"),
        stringsAsFactors = FALSE),
      metric = list(rmse = Metrics::rmse, auc = Metrics::auc))
  }, NA)

  df_perf

  expect_equal(dim(df_perf), c(12, 4))
  expect_true(all(df_perf$auc > .9))
  expect_true(all(c("rmse", "auc") %in% colnames(df_perf)))
  expect_equal(sum(is.na(df_perf)), 0)
})
