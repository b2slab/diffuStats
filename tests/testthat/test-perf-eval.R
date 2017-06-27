context("Perforance evaluation and data reshaping")

# toy vector, matrix and list
v_dummy <- setNames(1:26, LETTERS)
mat_dummy <- matrix(v_dummy, ncol = 1)
rownames(mat_dummy) <- names(v_dummy)
colnames(mat_dummy) <- "X1"
list_dummy <- list(X1 = mat_dummy)

test_that("Format recognition", {
  expect_equal({which_format(v_dummy)}, "vector")
  expect_equal({which_format(mat_dummy)}, "matrix")
  expect_equal({which_format(list_dummy)}, "list")
})

test_that("Format convertion", {
  expect_equal({which_format(to_list(v_dummy))}, "list")
  expect_equal({which_format(to_list(mat_dummy))}, "list")
  expect_equal({which_format(to_list(list_dummy))}, "list")

  expect_equal({which_format(to_x_from_list(list_dummy, "matrix"))}, "matrix")
  expect_equal({which_format(to_x_from_list(list_dummy, "vector"))}, "vector")

  expect_equal({to_x_from_list(to_list(v_dummy), "vector")}, v_dummy)
  expect_equal({to_x_from_list(to_list(mat_dummy), "matrix")}, mat_dummy)
})


# Define easy responses and predictions
mat1 <- matrix(c(.6, .7, 0.2, .4, .2, .6), ncol = 2)
rownames(mat1) <- paste0("V", 1:nrow(mat1))
colnames(mat1) <- paste0("S", 1:ncol(mat1))

mat2 <- matrix(c(1, 1, 0, 1, 0, 0), ncol = 2)
rownames(mat2) <- paste0("V", 1:nrow(mat2))
colnames(mat2) <- paste0("S", 1:ncol(mat2))

v1 <- setNames(mat1[, 1], rownames(mat1))
v2 <- setNames(mat2[, 1], rownames(mat2))

scores <- list(
  bkgd1 = mat1,
  bkgd2 = mat2
)
validation <- list(
  bkgd1 = mat2,
  bkgd2 = mat2
)

metrics <- list(
  auc = Metrics::auc,
  rmse = Metrics::rmse,
  logloss = Metrics::logLoss
)

test_that("Format convertion", {
  # with lists
  expect_error({perf_scores <- perf_eval(scores, validation, metrics)}, NA)

  # check columns
  expect_true({
    all(
      c("Column",
        "Background",
        "auc",
        "rmse",
        "logloss") %in% colnames(perf_scores))
  })
  expect_equal(dim(perf_scores), c(4, 5))



  # with matrices
  expect_error({perf_scores <- perf_eval(mat1, mat2, metrics)}, NA)
  expect_true({
    all(
      c("Column",
        "auc",
        "rmse",
        "logloss") %in% colnames(perf_scores))
  })
  expect_false({
    "Background" %in% colnames(perf_scores)
  })
  expect_equal(dim(perf_scores), c(2, 4))

  # with vectors
  expect_error({perf_scores <- perf_eval(v1, v2, metrics)}, NA)
  expect_true({
    all(
      c("auc",
        "rmse",
        "logloss") %in% colnames(perf_scores))
  })
  expect_false({
    any(c("Background", "Column") %in% colnames(perf_scores))
  })
  expect_equal(dim(perf_scores), c(1, 3))
})
