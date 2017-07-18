context("Kernels and kernel helpers")

test_that("Kernel checker works", {
  expect_true(is_kernel(diag(1:20)))
  expect_true(is_kernel(diag(0:20)))
  expect_false(is_kernel(diag(-1:20)))
})

data("graph_toy")

test_that("Kernel generators generate valid kernels", {
  expect_true(is_kernel(regularisedLaplacianKernel(graph_toy)))
  expect_true(is_kernel(diffusionKernel(graph_toy)))
  expect_true(is_kernel(pStepKernel(graph_toy)))
  expect_true(is_kernel(commuteTimeKernel(graph_toy)))
  expect_true(is_kernel(inverseCosineKernel(graph_toy)))
})

test_that("Diffusion function accepts kernels", {
  expect_error({
    scores <- diffuse(
      scores = graph_toy$input_mat,
      method = "raw",
      K = regularisedLaplacianKernel(graph_toy))
  }, NA)
  expect_true(all(scores > 0))
})

