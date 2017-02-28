library(igraph)

# Grid dimensions
nx <- 4*2
ny <- 3*2
seed <- 1

set.seed(seed)

# Graphical parameters
g <- diffusion::generate_graph(
  make_lattice,
  list(dimvector = c(nx, ny)),
  class_prop = c(source=0, filler=0, end=01))
V(g)$name <- paste0("A", 1:vcount(g))
g$layout <- layout.grid(g, width = nx, height = ny)
g$asp = ny/nx
V(g)$size = 20
plot(g)

# Four toy cases
input <- matrix(nrow = nx*ny, ncol = 4, data = 0)
input[1, 1] <- 1
input[1:nx, 2] <- 1
input[sample(nx*ny, nx*ny/10), 3] <- 1
input[sample(nx*ny, nx*ny/2), 4] <- 1
rownames(input) <- paste0("A", 1:nrow(input))
colnames(input) <- c("Single", "Row", "Small_sample", "Large_sample")

# Diffusion scores
output_raw <- diffusion::diffuse_raw(
  graph = g,
  scores = list(bkgd1 = input))$bkgd1
output_mc <- diffusion::diffuse_mc(
  graph = g,
  scores = list(bkgd1 = input),
  sample.prob = NULL)$bkgd1

# Save them to graph
g$input <- input
g$output <- output_raw

methods_raw <- c("raw", "ml", "gm", "ber_s", "ber_p", "mc", "z")
output <- plyr::llply(
  setNames(methods_raw, methods_raw),
  function(method) {
    ans <- diffusion::diffuse(
      graph = g,
      scores = list(bkgd1 = input),
      method = method)$bkgd1
    ans
  }
)


# Plot diffusion scores
par(ask = TRUE)
# plyr::l_ply(
#   colnames(output_raw),
#   function(score) {
#     plot(
#       g,
#       vertex.color = diffusion::scores2colours(output_raw[, score]),
#       vertex.shape = diffusion::scores2shapes(input[, score]),
#       main = score)
#   }
# )
# plyr::l_ply(
#   colnames(output_mc),
#   function(score) {
#     plot(
#       g,
#       vertex.color = diffusion::scores2colours(output_mc[, score]),
#       vertex.shape = diffusion::scores2shapes(input[, score]),
#       main = score)
#   }
# )
score_col <- 4
plyr::l_ply(
  methods_raw,
  function(method) {
    plot(
      g,
      vertex.color = diffusion::scores2colours(output[[method]][, score_col]),
      vertex.shape = diffusion::scores2shapes(input[, score_col]),
      main = paste0(
        "Method: ", method, "  Score: ",
        colnames(input)[score_col]))
  }
)
par(ask = FALSE)

# Save graph as package data
graph_toy <- g
save(graph_toy, file = "data/graph_toy.RData")
