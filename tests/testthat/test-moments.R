context("Functions to get exact moments")

K <- regularisedLaplacianKernel(graph_toy)
ids <- rownames(K)
ids.half <- head(ids, ncol(K)/2)

# original functions: 
# https://github.com/b2slab/diffuBench/blob/master/helper_funs.R
# 
# the kernel must be subsetted before applying the old functions!
# conversely, the ones in diffuStats have an argument to indicate 
# the ids of the labelled nodes
get_mu_old <- function(K, mu_y) {
  mu_y*rowSums(K)
}
get_covar_old <- function(K, var_y) {
  Kn <- K - rowMeans(K)
  var_y*tcrossprod(Kn)
}
get_ebias_old <- function(K) {
  get_mu_old(K, mu_y = 1)
}
get_vbias_old <- function(K) {
  n <- ncol(K)
  log10(apply(K, 1, var)*(n - 1))
}

test_that("Expected values", {
  # test against original functions
  # reference
  expect_equivalent(
    get_mu_reference(K, id_labelled = ids.half), 
    get_ebias_old(K[, ids.half, drop = FALSE])
  )
  
  # for the RL kernel... the values should be constant without unlabelled nodes
  # and equal to 1
  expect_equivalent(
    get_mu_reference(K, id_labelled = ids), 
    rep(1, nrow(K))
  )
  
  # actual expected value
  mu_y <- 3.14
  expect_equivalent(
    get_mu(K, id_labelled = ids.half, mu_y = mu_y), 
    get_mu_old(K[, ids.half, drop = FALSE], mu_y = mu_y)
  )
})

test_that("(Co)Variances", {
  # test against original functions
  # reference
  expect_equivalent(
    get_var_reference(K, id_labelled = ids.half), 
    get_vbias_old(K[, ids.half, drop = FALSE])
  )
  
  # actual expected value
  var_y <- 1.61
  expect_equivalent(
    get_covar(K, id_labelled = ids.half, var_y = var_y), 
    get_covar_old(K[, ids.half, drop = FALSE], var_y = var_y)
  )
})
