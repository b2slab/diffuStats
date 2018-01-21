#' Diffuse scores on a network
#'
#' @description 
#' Function \code{diffuse} takes a network in
#' \pkg{igraph} format (or a graph kernel matrix
#' stemming from a graph) and an initial state
#' to score all the nodes in the network.
#' The seven diffusion scores hereby provided differ on 
#' (a) how they distinguish positives, negatives and unlabelled examples, 
#' and (b) their statistical normalisation.
#' The argument \code{method} offers the following options:
#' 
#' Methods without statistical normalisation:
#' \itemize{
#' \item \code{raw}: positive nodes introduce unitary flow 
#' (\code{y_raw[i] = 1}) to the 
#' network, whereas neither negative nor unlabelled nodes 
#' introduce anything (\code{y_raw[j] = 0})
#' [Vandin, 2011]. 
#' They are computed as:
#' \deqn{ 
#' f_{raw} = K\cdot y_{raw}
#' }{
#' f_{raw} = K y_{raw}
#' }
#' where \code{K} is a graph kernel, see \code{?kernels}.
#' These scores treat negative and unlabelled nodes 
#' equivalently.
#' \item \code{ml}: same as \code{raw}, but negative nodes introduce a 
#' negative unit of flow [Zoidi, 2015] and are therefore not equivalent 
#' to unlabelled nodes.
#' \item \code{gm}: same as \code{ml}, but the unlabelled nodes are assigned 
#' a (generally non-null) bias term based on the total number of positives, 
#' negatives and unlabelled nodes [Mostafavi, 2008].
#' \item \code{ber_s}: this is a quantification of the relative 
#' change in the node score before and after the network smoothing. 
#' The score for a particular node \code{i} can be written as 
#' \deqn{ 
#' f_{ber_s, i} = \frac{f_{raw, i}}{y_{raw, i} + \epsilon} 
#' }{
#' f_{ber_s}[i] = f_{raw}[i]/(y_{raw}[i] + eps)
#' }
#' where \code{eps} is a parameter controlling the importance of the relative
#' change.
#' }
#' 
#' Methods with statistical normalisation: the \code{raw} diffusion score 
#' of every node \code{i} is computed and compared to 
#' its own diffusion scores stemming from a permuted input. 
#' \itemize{
#' \item \code{mc}: the score of node \code{i} is based in 
#' its empirical p-value, 
#' computed by permuting the input \code{n.perm} times:
#' \deqn{
#' p_i = \frac{r_i + 1}{n.perm + 1}
#' }{
#' p[i] = (r[i] + 1)(n.perm + 1)
#' }
#' \code{p[i]} is roughly the proportion of input permutations
#' that led to a diffusion score as high or higher than the
#' original diffusion score
#' (a total of \code{r[i]} for node \code{i}, in absolute terms).
#' This assesses how likely a high diffusion score is to arise 
#' from chance, in absence of signal. 
#' To be consistent with the direction, \code{mc} is defined as:
#' \deqn{
#' f_{mc, i} = 1 - p_i
#' }{
#' f_{mc}[i] = 1 - p[i]
#' }
#' \item \code{ber_p}: as used in [Bersanelli, 2016], this 
#' score combines \code{raw} and \code{mc}, in order to take into 
#' account both the magnitude of the \code{raw} scores and the 
#' effect of the network topology:
#' \deqn{
#' f_{ber_p, i} = -\log_{10}(p_i)\cdot f_{raw, i}
#' }{
#' f_{ber_p}[i] = -log10(p[i]) f_{raw}[i]
#' }
#' \item \code{z}: this is a parametric alternative to \code{mc}. 
#' The \code{raw} score of node \code{i} is subtracted its mean 
#' value and divided by its standard deviation. 
#' The statistical moments have a closed analytical form, 
#' see the main vignette, and are inspired in [Harchaoui, 2013].
#' Unlike \code{mc} and \code{ber_p}, the \code{z} scores do not 
#' require actual permutations, giving them an advantage in terms of speed.
#' }
#' 
#' If the input labels are not quantitative, i.e. positive(1), negative(0) 
#' and possibly unlabelled, all the scores (\code{raw}, \code{gm},
#' \code{ml}, \code{z}, \code{mc}, \code{ber_s}, \code{ber_p}) can be used. 
#' Quantitative inputs are naturally defined on 
#' \code{raw}, \code{z}, \code{mc}, \code{ber_s} and \code{ber_p}
#' by extending the definitions above, and are readily available 
#' in \code{diffuStats}.
#' Further details on the scores can be found in the main vignette.
#' 
#' @details 
#' Input scores can be specified in three formats.
#' A single set of scores to smooth can be represented as (1) a named
#' numeric vector, whereas if several of these vectors that share
#' the node names need to be smoothed, they can be provided as
#' (2) a column-wise matrix. However, if the unlabelled
#' entities are not the same from one case to another, 
#' (3) a named list of such score
#' matrices can be passed to this function. The input format will
#' be kept in the output.
#' 
#' The implementation of \code{mc} and \code{ber_p} is optimized 
#' for sparse inputs. 
#' Dense inputs might take a longer time to compute.
#' Another relevant note: \code{z} can give NaN for a particular node
#' when the observed nodes are disconnected from the node being scored.
#' This is because these nodes are neither annotated with experimental 
#' not network (topology) data.
#'
#' @param graph \pkg{igraph} object for the diffusion.
#' Alternatively, a kernel matrix can be provided through the 
#' argument \code{K} insted of the igraph object.
#' @param scores scores to be smoothed; either a named numeric vector,
#' a column-wise matrix whose rownames are nodes and colnames are
#' different scores, or a named list of such matrices.
#' @param method character, one of \code{raw}, \code{gm},
#' \code{ml}, \code{z}, \code{mc}, \code{ber_s}, \code{ber_p}. 
#' For batch analysis of several methods, see \code{?diffuse_grid}.
#' @param ... additional arguments for the diffusion method. 
#' \code{mc} and \code{ber_p} accept \code{n.perm} (number of permutations), 
#' \code{seed} (for reproducibility, defaults to \code{1}) and 
#' \code{sample.prob}, a list of named vectors -one per background- 
#' with sampling probabilities for the null model, uniform by default. 
#' More details available in \code{?diffuse_mc}. 
#' On the other hand, \code{ber_s} accepts \code{eps}, a parameter 
#' controlling the importance of the relative change.
#'
#' @return \code{diffuse} returns the diffusion scores, 
#' with the same format as \code{scores}
#'
#' @references 
#' Scores "raw":
#' Vandin, F., Upfal, E., & Raphael, B. J. (2011). 
#' Algorithms for detecting significantly mutated pathways in cancer. 
#' Journal of Computational Biology, 18(3), 507-522.
#' 
#' Scores "ml":
#' Zoidi, O., Fotiadou, E., Nikolaidis, N., & Pitas, I. (2015). 
#' Graph-based label propagation in digital media: A review. 
#' ACM Computing Surveys (CSUR), 47(3), 48.
#' 
#' Scores "gm":
#' Mostafavi, S., Ray, D., Warde-Farley, D., Grouios, C., & Morris, Q. (2008). 
#' GeneMANIA: a real-time multiple association network integration algorithm 
#' for predicting gene function. 
#' Genome biology, 9(1), S4.
#' 
#' Scores "mc", "ber_s", "ber_p":
#' Bersanelli, M., Mosca, E., Remondini, D., 
#' Castellani, G., & Milanesi, L. (2016). 
#' Network diffusion-based analysis of high-throughput data 
#' for the detection of differentially enriched modules. 
#' Scientific reports, 6.
#' 
#' Scores "z":
#' Harchaoui, Z., Bach, F., Cappe, O., & Moulines, E. (2013). 
#' Kernel-based methods for hypothesis testing: A unified view. 
#' IEEE Signal Processing Magazine, 30(4), 87-97.
#'
#' @examples
#' ##############################
#' 
#' library(igraph)
#' library(ggplot2)
#' data(graph_toy)
#' input_vec <- graph_toy$input_vec
#' n <- vcount(graph_toy)
#' 
#' ##############################
#' 
#' # Examples for 'diffuse':
#' 
#' # Using a binary vector as input
#' diff_scores <- diffuse(
#'     graph = graph_toy,
#'     scores = input_vec,
#'     method = "raw")
#' 
#' # Using a matrix as input
#' diff_scores <- diffuse(
#'     graph = graph_toy,
#'     scores = graph_toy$input_mat,
#'     method = "raw")
#' 
#' # Using a list of matrices as input
#' diff_scores <- diffuse(
#'     graph = graph_toy,
#'     scores = list(myScores1 = graph_toy$input_mat,
#'         myScores2 = head(graph_toy$input_mat, n/2)),
#'     method = "raw")
#' 
#' ##############################
#' 
#' # Examples for 'diffuse_grid':
#' 
#' # Using a single vector of scores and comparing the methods 
#' # "raw", "ml", and "z"
#' df_diff <- diffuse_grid(
#'     graph = graph_toy,
#'     scores = graph_toy$input_vec,
#'     grid_param = expand.grid(method = c("raw", "ml", "z")))
#' head(df_diff)
#' 
#' # Same settings, but comparing several choices of the 
#' # parameter epsilon ("eps") in the scores "ber_s"
#' df_diff <- diffuse_grid(
#'     graph = graph_toy,
#'     scores = graph_toy$input_vec,
#'     grid_param = expand.grid(method = "ber_s", eps = 1:5/5))
#' ggplot(df_diff, aes(x = factor(eps), fill = eps, y = node_score)) + 
#'     geom_boxplot()
#' 
#' # Using a matrix with four set of scores
#' # called Single, Row, Small_sample, Large_sample
#' # See the 'quickstart' vignette for more details on these toy scores
#' # We compute scores for methods "ber_p" and "mc" and 
#' # permute both 1e3 and 1e4 times in each run
#' df_diff <- diffuse_grid(
#'     graph = graph_toy,
#'     scores = graph_toy$input_mat,
#'     grid_param = expand.grid(
#'         method = c("mc", "ber_p"), 
#'         n.perm = c(1e3, 1e4)))
#' dim(df_diff)
#' head(df_diff)
#' 
#' ##############################
#' 
#' # Differences when using (1) a quantitative input and
#' # (2) different backgrounds. 
#' 
#' # In this example, the 
#' # small background contains binary scores and continuous scores for 
#' # half of the nodes in the 'graph_toy' example graph. 
#' 
#' # (1) Continuous scores have been generated by 
#' # changing the positive labels to a random, positive numeric value. 
#' # The user can see the impact of this in the scores 'raw', 'ber_s', 
#' # 'ber_p', 'mc' and 'z'
#' 
#' # (2) The larger background is just the small background 
#' # completed with zeroes, both for binary and continuous scores. 
#' # This illustrates how 'raw' and 'ber_s' treat unlabelled 
#' # and negative labels equally, whereas 'ml', 'gm', 'ber_p', 
#' # 'mc' and 'z' do not. 
#' 
#' # Examples:
#' 
#' # The input:
#' lapply(graph_toy$input_list, head)
#' 
#' # 'raw' scores treat equally unlabelled and negative nodes, 
#' # and can account for continuous inputs
#' diff_raw <- diffuse(
#'     graph = graph_toy,
#'     scores = graph_toy$input_list,
#'     method = "raw")
#' lapply(diff_raw, head)
#' 
#' # 'z' scores distinguish unlabelled and negatives and accepts 
#' # continuous inputs
#' diff_z <- diffuse(
#'     graph = graph_toy,
#'     scores = graph_toy$input_list,
#'     method = "z")
#' lapply(diff_z, head)
#' 
#' # 'ml' and 'gm' are the same score if there are no unobserved nodes
#' diff_compare <- diffuse_grid(
#'     graph = graph_toy, 
#'     scores = input_vec, 
#'     grid_param = expand.grid(method = c("raw", "ml", "gm"))
#' )
#' df_compare <- reshape2::acast(
#'     diff_compare, 
#'     node_id~method, 
#'     value.var = "node_score")
#' head(df_compare)
#' 
#' # 'ml' and 'gm' are different in presence of unobserved nodes
#' diff_compare <- diffuse_grid(
#'     graph = graph_toy, 
#'     scores = head(input_vec, n/2), 
#'     grid_param = expand.grid(method = c("raw", "ml", "gm"))
#' )
#' df_compare <- reshape2::acast(
#'     diff_compare, 
#'     node_id~method, 
#'     value.var = "node_score")
#' head(df_compare)
#' 
#' @import igraph
#' @export
diffuse <- function(
    graph,
    scores,
    method,
    ...) {
    # check sanity
    .check_method(method)

    # For data reshaping
    format_scores <- which_format(scores)
    scores <- to_list(scores)

    # Check if we have a graph or a kernel
    if (!missing("graph")) {
        format_network <- "graph"
    } else {
        if (!("K" %in% names(list(...))))
            stop("Neither a graph 'graph' or a kernel 'K' were provided")
        format_network <- "kernel"
    }

    if (method == "raw") {
        ans <- (diffuse_raw(graph = graph, scores = scores, ...))
    }
    if (method == "ml") {
        scores_ml <- lapply(
            scores,
            function(mat) {
                if (!all(as.numeric(mat) %in% c(0, 1))) 
                    stop("Input scores for ", method, " must be binary")
                mat[mat == 0] <- -1
                mat
            }
        )
        ans <- (diffuse_raw(graph = graph, scores = scores_ml, ...))
    }
    if (method == "gm") {
        scores_gm <- lapply(
            scores,
            function(mat) {
                if (!all(as.numeric(mat) %in% c(0, 1))) 
                    stop("Input scores for ", method, " must be binary")
                # Have to match rownames with background
                # If the kernel is provided...
                if (format_network == "graph") {
                    names_ordered <- V(graph)$name
                } else if (format_network == "kernel") {
                    names_ordered <- rownames(list(...)[["K"]])
                }
                
                # If the graph is defined...
                ids_nobkgd <- setdiff(names_ordered, rownames(mat))
                n_tot <- length(names_ordered)
                n_bkgd <- nrow(mat)
                n_col <- ncol(mat)

                # normalisation has to be performed
                # for each column, as it depends
                # on the number of positives and negatives...
                # n_pos and n_neg are vectors counting the number of 
                # positives and negatives in each column
                n_pos <- colSums(mat)
                n_neg <- n_bkgd - n_pos
                # biases
                p <- (n_pos - n_neg)/n_tot
                
                mat[mat == 0] <- -1
                # add biases (each column has its bias)
                mat.rbind <- matrix(
                    nrow = n_tot - n_bkgd, 
                    ncol = n_col, 
                    data = rep(p, each = n_tot - n_bkgd))
                rownames(mat.rbind) <- ids_nobkgd
                
                mat <- rbind(as.matrix(mat), mat.rbind)
                
                # sort the names as in the original graph
                mat[names_ordered, , drop = FALSE]
            }
        )
        ans <- (diffuse_raw(graph = graph, scores = scores_gm, ...))
    }

    # Monte-Carlo simulations
    if (method == "mc") {
        ans <- (diffuse_mc(graph = graph, scores = scores, ...))
    }

    # z scores
    if (method == "z") {
        ans <- (diffuse_raw(graph = graph, scores = scores, z = TRUE, ...))
    }

    # Bersanelli's scores (s)
    if (method == "ber_s") {
        list_dots <- list(...)

        if ("eps" %in% names(list_dots)) {
            eps <- list_dots$eps
        } else {
            eps <- 1
        }
        # Compute final state
        scores_raw <- diffuse_raw(graph = graph, scores = scores, ...)

        scores_ber_s <- plyr::llply(
            setNames(names(scores), names(scores)),
            function(scores_name) {
                # each list can have a different background...
                # nodes outside the background will be
                # assigned a prior score of 0
                mat_out <- scores_raw[[scores_name]]
                mat_in <- scores[[scores_name]]

                # matrix with correct dimnames but populated with eps
                mat_in_fill <- Matrix::Matrix(
                    data = eps,
                    nrow = nrow(mat_out),
                    ncol = ncol(mat_out),
                    dimnames = dimnames(mat_out))

                # add the original input: only in the rows that match
                mat_in_fill[rownames(mat_in), ] <-
                    mat_in_fill[rownames(mat_in), ] + mat_in

                as.matrix(mat_out/mat_in_fill)
            }
        )
        ans <- (scores_ber_s)
    }

    # Bersanelli's scores (p)
    if (method == "ber_p") {
        # Compute final state
        scores_raw <- diffuse_raw(graph = graph, scores = scores, ...)

        # Compute p
        scores_mc <- diffuse_mc(
            graph = graph,
            scores = scores,
            oneminusHeatRank = FALSE,
            ...)

        scores_ber_p <- plyr::llply(
            setNames(names(scores), names(scores)),
            function(scores_name) {
                s_mc <- scores_mc[[scores_name]]
                s_raw <- scores_raw[[scores_name]]

                -log10(s_mc)*s_raw
            }
        )
        ans <- (scores_ber_p)
    }
    if (!exists("ans")) {
        message(
            "The specified method ",
            method,
            " is not implemented and will return NULL")
        return(invisible())
    }

    message("All done")
    # reshape back to original and return
    return(to_x_from_list(ans, format_scores))
}
