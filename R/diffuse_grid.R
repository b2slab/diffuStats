#' Function \code{diffuse_grid} computes diffusion scores on a grid of
#' parameters. It is a convenient wrapper on \code{diffuse} that 
#' takes a network in
#' \code{\link[igraph]{igraph}} format or a kernel, initial scores
#' to compute the diffusion scores for all the nodes in the network 
#' and a grid of parameters to 
#' explore. The diffusion scores are computed for every combination 
#' of parameters provided and returned in a long-format data frame.
#' 
#' @param grid_param data frame containing parameter combinations to explore.
#' The column names should be the names of the parameters. 
#' Parameters that have a fixed value can be specified in the grid 
#' or through the additional arguments (\code{...})
#' 
#' @return \code{diffuse_grid} returns a data frame 
#' containing the diffusion scores for the specified 
#' combinations of parameters
#'
#' @rdname diffuse
#' 
#' @import plyr
#' @export
diffuse_grid <- function(
    scores,
    grid_param,
    ...) {

    # parameter names
    names_param <- colnames(grid_param)

    # data format
    data_format <- which_format(scores)
    
    plyr::adply(
        grid_param,
        1,
        function(row_param) {
            # params for do.call
            list_param <- as.list(setNames(row_param, names_param))

            # diffuse
            scores_param <- do.call(
                diffuse,
                c(list(...), list(scores = scores), list_param)
            )
            
            # depending on the input format, return a data frame with columns
            # "node_score" (vector), original colnames (matrix) and 
            # "background" plus the original colnames (list)
            # Also, it is important to have the "node_id" column in each case!
            if (data_format == "vector") {
                return(data.frame(
                    node_id = names(scores_param), 
                    node_score = scores_param))
            } 
            if (data_format == "matrix") {
                return(data.frame(
                    node_id = rownames(scores_param), 
                    scores_param
                ))
            }
            if (data_format == "list") {
                names_nodes <- rownames(scores_param[[1]])
                return(data.frame(
                    node_id = names_nodes, 
                    plyr::ldply(
                        scores_param, 
                        identity, 
                        .id = "background"
                    )
                ))
            }
        }
    )
}
