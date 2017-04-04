#' Generate a random input for graph diffusion
#'
#' Function \code{generate_input} generates a random list of
#' nodes from an \code{\link[igraph]{igraph}} object.
#' It also specifies the true solution generating the
#' list. The graph object needs to have some attributes
#' (automatically added through \code{generate_graph})
#'
#' @param graph an \code{\link[igraph]{igraph}} object, typically from
#' \code{\link[diffusion]{generate_input}}
#' @param order numeric or vector, order of the neighbourhoods that generate
#' the list
#' @param length_inputs numeric, number of nodes in the generated inputs
#' @param return_matrix logical, should inputs be returned as a matrix?
#' @param seed numeric, seed for random number generator
#'
#' @return A list whose elements are lists with three slots:
#' \code{pos} for the true signal generators, \code{neg} for the
#' nodes that did not generate signal and \code{input} for the
#' signal itself
#'
#' @examples
#' g <- generate_graph(
#'     fun_gen = igraph::barabasi.game,
#'     param_gen = list(n = 200, m = 3, directed = FALSE),
#'     seed = 1)
#' synth_input <- generate_input(
#'     g,
#'     order = 2,
#'     length_inputs = 3, return_matrix = TRUE)
#' str(synth_input)
#'
#' @importFrom plyr llply
#' @import igraph
#' @export
generate_input <- function(
    graph,
    order,
    length_inputs,
    return_matrix = TRUE,
    seed = NULL) {

    if (!is.null(seed)) set.seed(seed)

    # possible solutions
    id.source <- as.numeric(V(graph)[class == "source"])
    id.end <- as.numeric(V(graph)[class == "end"])

    if (is.null(names(order))) names(order) <- paste0("X", seq_along(order))
    ans <- plyr::llply(
        order,
        function(ord) {
            input <- numeric(0)
            n_iter <- 1

            source.chosen <- numeric(0)
            # keep adding sources until full
            while(length(input) < length_inputs)  {
                n_iter <- n_iter + 1
                if (n_iter > 1e3) {
                    stop(
                        "No feasible input was found using ",
                        "input parameters after 1e3 iterations")
                }

                # random source
                source.now <- sample(id.source, 1)
                # its neighbours
                v.neighbors <- as.numeric(
                    neighborhood(
                        graph = graph,
                        nodes = source.now,
                        order = ord,
                        mindist = 1)[[1]])
                # eligible neighbours
                v.neighbors <- intersect(v.neighbors, id.end)
                # sample the nodes in the input
                if (length(v.neighbors) > 0) {
                    input.now <- sample(
                        # for the case of a single integer
                        c(v.neighbors, v.neighbors),
                        size = length_inputs,
                        replace = TRUE)

                    source.chosen <- union(source.chosen, source.now)
                    input <- union(input, input.now)
                }
                }

            # input could have more than the desired entries, trim it
            input <- head(input, length_inputs)

            # positive and negative sources
            pos <- source.chosen
            neg <- setdiff(id.source, pos)

            # # if we ask for names and they are available, return them
            # if (return_names & "name" %in% list.vertex.attributes(graph)) {
            #   return(
            #     list(
            #       input = V(graph)[input]$name,
            #       pos = V(graph)[pos]$name,
            #       neg = V(graph)[neg]$name))
            # }
            # otherwise return vertex ids
            return(
                list(
                    input = input,
                    pos = pos,
                    neg = neg))
        }
    )

    if (return_matrix) {
        # matrix for input
        mat_input <- t(plyr::laply(
            ans,
            function(iter) {
                (id.end %in% iter$input)*1
            },
            .drop = FALSE
        ))
        if ("name" %in% list.vertex.attributes(graph)) {
            rownames(mat_input) <- V(graph)[id.end]$name
        } else {
            rownames(mat_input) <- id.end
        }

        colnames(mat_input) <- names(order)

        # matrix for ground truth
        mat_source <- t(plyr::laply(
            ans,
            function(iter) {
                (id.source %in% iter$pos)*1
            },
            .drop = FALSE
        ))
        if ("name" %in% list.vertex.attributes(graph)) {
            rownames(mat_source) <- V(graph)[id.source]$name
        } else {
            rownames(mat_source) <- id.source
        }
        colnames(mat_source) <- names(order)

        return(list(mat_input = mat_input, mat_source = mat_source))
    } else {
        return(ans)
    }
}
