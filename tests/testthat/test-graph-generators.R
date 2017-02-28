context("Graph generator")

library(igraph)

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
  class_prop <- c(source = 50, filler = 450, end = 500)
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
        lst$class_prop = class_prop
        g <- do.call(generate_graph, lst)
      }, NA)
      # ALl must be connected, with 3 vertices and with vertex class
      expect_true(is.connected(g))
      expect_equal(vcount(g), 1000)
      expect_length(table(V(g)$class), 3)
    }
  )
})
