context("Graph generator")

library(igraph)

set.seed(1)

test_that("helpers", {
  # Generate disconnected graph
  g <- graph.empty(n = 100, directed = FALSE) +
    graph.full(100, directed = FALSE)
  g_con <- .connect_undirected_graph(g)

  expect_true({
    is.connected(g_con)
  })
})

test_that("generate_graph", {
  list_params <- list(
    barabasi = list(
      fun_gen = barabasi.game,
      param_gen = list(n = 1000, m = 5, directed = F)
    ),
    lattice = list(
      fun_gen = graph.lattice,
      param_gen = list(length = 10, dim = 3, directed = F)
    ),
    watts = list(
      fun_gen = watts.strogatz.game,
      param_gen = list(
        dim = 3, size = 10, nei = 1,
        p = .1, loops = F, multiple = F)
    ),
    erdos = list(
      fun_gen = erdos.renyi.game,
      param_gen = list(
        n = 1000, p.or.m = 3000, type = "gnm",
        directed = F, loops = F)
    )
  )

  plyr::l_ply(
    list_params,
    function(lst) {
      # Generate graph flawlessly
      expect_error({
        g <- do.call(generate_graph, lst)
      }, NA)
      # All must be connected, with 3 vertices and with vertex class
      expect_true(is.connected(g))
      expect_equal(vcount(g), 1000)
      expect_length(table(V(g)$class), 3)
    }
  )
})

test_that("generate_input", {
  # generate graph flawlessly
  expect_error({
    g <- generate_graph(
      fun_gen = barabasi.game,
      param_gen = list(n = 1000, m = 5, directed = F)
    )
  }, NA)

  # Check that scores are not constant, meaning that they have some
  # 0s and 1s
  list_ord <- 1:5
  n_rep <- 100
  plyr::llply(
    list_ord,
    function(ord) {
      expect_error({
        g_in <- generate_input(
          g,
          order = setNames(rep(1, length.out = n_rep), paste0("Case", 1:n_rep)),
          length_inputs = 50, return_matrix = TRUE
        )
      }, NA)
      expect_false(any(apply(g_in$mat_input, 2, sd) == 0))
      expect_false(any(colSums(g_in$mat_source) == 0))
    }
  )
})
