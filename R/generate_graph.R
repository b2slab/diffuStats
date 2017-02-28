#' Generate data.frame with default vertex attributes
#'
#' @rdname default_graph_param
#'
# #' @importFrom ggsci pal_npg
#' @import igraph
.default_graph_param <- function(){
  data.frame(
    # class = c("source", "filler", "end"),
    # color = ggsci::pal_npg()(3),
    color = c("#E64B35FF", "#4DBBD5FF", "#00A087FF"),
    shape = "circle",
    frame.color = "gray20",
    label.color = "gray5",
    size = 12,
    stringsAsFactors = FALSE,
    row.names = c("source", "filler", "end")
  )
}

#' Function to connect a non connected graph
#'
#' @param g an igraph object
#'
#' @rdname connect_undirected_graph
#'
#' @import igraph
.connect_undirected_graph <- function(g) {
  # browser()
  g.clusters <- clusters(g)

  # Partition nodes: in/out largest cc
  nodes.largest.cc <- which(
    g.clusters$membership == which.max(g.clusters$csize))
  nodes.addEdges <- which(
    g.clusters$membership != which.max(g.clusters$csize))

  if (length(nodes.addEdges) == 0) return(g)

  # For each node out, add one edge to a random node in
  g.newEdges <- sapply(
    nodes.addEdges,
    function(dummy) sample(nodes.largest.cc, 1))

  g <- add.edges(
    graph = g,
    edges = rbind(V(g)[nodes.addEdges], V(g)[g.newEdges]),
    directed = F)

  g
}

#' Generate a random graph
#'
#' Function \code{generate_graph} generates a random network
#' using \code{\link[igraph]{igraph}} graph generators. Several models are
#' available, and
#'
#' @param fun_gen function to generate the graphs. Typically from
#' \code{\link[igraph]{igraph}}, like \code{\link[igraph]{barabasi.game}},
#' \code{\link[igraph]{watts.strogatz.game}},
#' \code{\link[igraph]{erdos.renyi.game}},
#' \code{\link[igraph]{make_lattice}}, etc.
#' @param param_gen list with parameters to pass to \code{fun_gen}
#' @param class_prop named character, proportions of each vertex class
#' (is internally normalised to sum up one). Default class names are
#' \code{c("source", "filler", "end")}.
#' @param class_attr data.frame with vertex classes as rownames and a column
#' for each vertex attribute. The name of the column will be used as the
#' attribute name.
#' @param fun_curate function to apply to the graph before returning it.
#' Can be set to \code{identity} or \code{NULL} to skip this step.
#' By default, the graph is connected: nodes not belonging to the
#' largest connected component are randomly wired to a node in it.
#'
#' @return An \code{\link[igraph]{igraph}} object
#'
#' @import igraph
#' @export
generate_graph <- function(
  fun_gen,
  param_gen,
  class_prop,
  class_attr = .default_graph_param(),
  fun_curate = .connect_undirected_graph
) {
  # browser()

  # Generate network
  g <- do.call(fun_gen, param_gen)
  n <- vcount(g)

  # Assign vertex classes
  class <- (class_prop/sum(class_prop))*n
  class_char <- character()
  for (i in names(class))
    class_char <- c(
      class_char,
      rep(i, class[i]))
  n_out <- length(class_char)
  if (n_out < n) {
    n_diff <- n - n_out
    class_char <- c(
      class_char,
      rep(utils::tail(names(class), 1), n_diff))
  }
  V(g)$class <- class_char

  # Assign vertex attributes
  for (col in names(class_attr)) {
    mapper <- stats::setNames(class_attr[[col]], rownames(class_attr))
    g <- set_vertex_attr(
      g,
      name = col,
      value = mapper[V(g)$class])
  }

  # Connect graph (optional)
  if (is.null(fun_curate)) return(g)
  else return(do.call(fun_curate, list(g)))
}
