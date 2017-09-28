context("Grid search for performance evaluation")

data("graph_toy")
set.seed(1)

metrics <- list(
    auc = metric_fun(curve = "ROC"),
    prc = metric_fun(curve = "PRC"))

test_that("Grid search works on a graph", {
  expect_error({
    df_perf <- perf(
      graph = graph_toy,
      scores = graph_toy$input_mat,
      validation = graph_toy$input_mat,
      grid_param = expand.grid(
        method = c("raw", "z", "mc"),
        stringsAsFactors = FALSE),
      metric = metrics)
  }, NA)

  df_perf

  expect_equal(dim(df_perf), c(12, 4))
  expect_true(all(df_perf$auc > .9))
  expect_true(all(names(metrics) %in% colnames(df_perf)))
  expect_equal(sum(is.na(df_perf)), 0)
  
  # cast the data 
  df_cast <- reshape2::acast(
      df_perf, formula = Column~method, value.var = names(metrics)[2])
  
  # compare aucs statistically
  # warnings supressed because metrics are very good and 
  # the test has a lot of ties -> it complains
  expect_error({
      mat_wilcox <- suppressWarnings(perf_wilcox(df_cast, ci = 0.50))
  }, NA)
  expect_type(mat_wilcox, "character")
})

test_that("Grid search works on a kernel", {
  expect_error({
    df_perf <- perf(
      K = regularisedLaplacianKernel(graph_toy),
      scores = graph_toy$input_mat,
      validation = graph_toy$input_mat,
      grid_param = expand.grid(
        method = c("raw", "z", "mc"),
        stringsAsFactors = FALSE),
      metric = metrics)
  }, NA)

  df_perf

  expect_equal(dim(df_perf), c(12, 4))
  expect_true(all(df_perf$auc > .9))
  expect_true(all(names(metrics) %in% colnames(df_perf)))
  expect_equal(sum(is.na(df_perf)), 0)
  
  # check perf_wilcox
})

test_that("perf_wilcox works", {
    df_wilcox <- cbind(
        met1 = runif(100, min = 0, max = 0.7), 
        met2 = runif(100, min = 0.3, max = 1)
    )
    expect_error({
        mat_wilcox <- perf_wilcox(df_wilcox, ci = 0.50)
    }, NA)
    expect_type(mat_wilcox, "character")
})