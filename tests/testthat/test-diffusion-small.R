context("Input transforms for diffusion")

library(igraph)
#library(magrittr)

set.seed(3)

# Generate disconnected graph...
# so that output equals input
g <- graph.empty(n = 5, directed = FALSE)
V(g)$name <- paste0("V", as.numeric(V(g)))

mat <- cbind(c(1, 0, 0), c(1, 1, 0))
rownames(mat) <- paste0("V", 1:nrow(mat))
colnames(mat) <- paste0("Input", 1:ncol(mat))

sol <- list(
  raw = cbind("Input1" = c(1, 0, 0, 0, 0),
              "Input2" = c(1, 1, 0, 0, 0)),
  ml = cbind("Input1" = c(1, -1, -1, 0, 0),
             "Input2" = c(1, 1, -1, 0, 0)),
  gm = cbind("Input1" = c(1, -1, -1, -.2, -.2),
             "Input2" = c(1, 1, -1, .2, .2)),
  # Computed by hand
  z = cbind("Input1" = c(sqrt(2), -sqrt(2)/2, -sqrt(2)/2, NaN, NaN),
            "Input2" = c(sqrt(2)/2, sqrt(2)/2, -sqrt(2), NaN, NaN))
)

methods_raw <- c("raw", "ml", "gm", "z")
methods_all <- c(methods_raw, "mc", "ber_s", "ber_p")

test_that("Transforms on the input are accurate", {
  # Diffusion does not fail
  out <- plyr::llply(
    setNames(methods_raw, methods_raw),
    function(method) {
      # message(method)
      expect_error({
        # if (method == "z") browser()
        ans <- diffuse(
          graph = g,
          scores = list(bkgd1 = mat),
          method = method)$bkgd1
      }, NA)
      ans
    }
  )

  # The transforms are correct
  plyr::l_ply(
    methods_raw,
    function(method)
      expect_equivalent(
        as.matrix(out[[method]]),
        sol[[method]])
  )
})

test_that("Input can be introduced as a vector", {
  # Use Input1
  vec_input <- setNames(mat[, "Input1"], rownames(mat))

  out <- plyr::llply(
    setNames(methods_raw, methods_raw),
    function(method) {
      # message(method)
      expect_error({
        # if (method == "z") browser()
        ans <- diffuse(
          graph = g,
          scores = vec_input,
          method = method)
      }, NA)
      ans
    }
  )

  # The transforms are correct
  plyr::l_ply(
    methods_raw,
    function(method)
      expect_equivalent(
        out[[method]],
        sol[[method]][, "Input1"])
  )
})

test_that("Input can be introduced as a matrix", {
  # Use mat for inputs
  mat_input <- mat

  out <- plyr::llply(
    setNames(methods_raw, methods_raw),
    function(method) {
      # message(method)
      expect_error({
        # if (method == "z") browser()
        ans <- diffuse(
          graph = g,
          scores = mat_input,
          method = method)
      }, NA)
      ans
    }
  )

  # The transforms are correct
  plyr::l_ply(
    methods_raw,
    function(method)
      expect_equivalent(
        (out[[method]]),
        sol[[method]])
  )
})

K <- regularisedLaplacianKernel(g)
set.seed(1)
test_that("Graph can be introduced as a kernel (deterministic)", {
  # Use mat for inputs
  mat_input <- mat

  # Check ALL the methods for execution errors
  out <- plyr::llply(
    setNames(methods_all, methods_all),
    function(method) {
      # message(method)
      expect_error({
        # if (method == "z") browser()
        ans <- diffuse(
          K = K,
          scores = mat_input,
          method = method)
      }, NA)
      ans
    }
  )

  # The transforms are correct (deterministic only)
  plyr::l_ply(
    methods_raw,
    function(method)
      expect_equivalent(
        (out[[method]]),
        sol[[method]])
  )
})
