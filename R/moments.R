#' @title Compute exact statistical moments
#'
#' @details These functions enable exploring the properties of the 
#' null distributions of diffusion scores. They provide 
#' the exact statistical moments mentioned in:
#' 
#' Sergio Picart-Armada, Wesley K Thompson, Alfonso Buil, 
#' Alexandre Perera-Lluna. The effect of statistical normalisation 
#' on network propagation scores. Bioinformatics, 2020, btaa896.
#' https://doi.org/10.1093/bioinformatics/btaa896
#' 
#' Specifically, \code{get_mu_reference()} and \code{get_var_reference()} 
#' provide the so-called 'Reference expected values' and 
#' 'Reference variances', which are input-independent (one only needs 
#' the kernel and the ids of the labelled nodes). Getting the actual 
#' expected values and variances requires providing the input expected 
#' value and variance, and can be achieved with \code{get_mu()} and 
#' \code{get_covar()}.
#' 
#' @param K square matrix, precomputed diffusion graph kernel, see ?kernels
#' @param id_labelled character, names of the labelled nodes (must be a subset
#' of the colnames of K)
#' @param mu_y,var_y (scalar) mean and variance of the input, see details
#' 
#' @return A kernel matrix with adequate dimnames
#'
#' @examples
#' data(graph_toy)
#' ## Kernel
#' K_pstep <- pStepKernel(graph_toy)
#' ## Labelled nodes
#' ids <- head(rownames(K_pstep), ncol(K_pstep)/3)
#' ## Reference values
#' get_mu_reference(K_pstep, ids)
#' get_var_reference(K_pstep, ids)
#' ## Actual moments with an input y
#' y <- graph_toy$input_vec[ids]
#' mu_y <- mean(y)
#' var_y <- var(y)
#' mu <- get_mu(K_pstep, ids, mu_y = mu_y)
#' covar <- get_covar(K_pstep, ids, var_y = var_y)
#' ## mean values
#' mu
#' ## variances
#' diag(covar)
#' ## covariances
#' covar[1:6, 1:6]
#' 
#' @name moments
#' @references 
#' Article: https://doi.org/10.1093/bioinformatics/btaa896
#' Functions: https://github.com/b2slab/diffuBench/blob/master/helper_funs.R
#' 
#' 
#' 
#' @description Function \code{get_mu()} computes the exact expected values
#' of the null distributions
#' 
#' @importFrom checkmate qassert assertMatrix assertTRUE assertSubset 
#' @export
get_mu <- function(K, id_labelled = colnames(K), mu_y) {
    # check mu is a scalar
    checkmate::qassert(mu_y, "N1")
    
    # check numeric, square, named matrix
    checkmate::assertMatrix(K, any.missing = FALSE, mode = "numeric")
    checkmate::assertTRUE(nrow(K) == ncol(K))
    
    # check that ids are a subset of the columns
    checkmate::assertSubset(id_labelled, colnames(K))
    
    K <- K[, unique(id_labelled), drop = FALSE]
    mu_y*rowSums(K)
}

#' @description Function \code{get_covar()} computes the exact covariance matrix
#' of the null distributions (square matrix, same size as kernel matrix); 
#' the variances are the values in the matrix diagonal
#' 
#' @name moments
#' @importFrom checkmate qassert assertMatrix assertTRUE assertSubset
#' @export
get_covar <- function(K, id_labelled = colnames(K), var_y) {
    # check mu is a scalar
    checkmate::qassert(var_y, "N1")
    
    # check numeric, square, named matrix
    checkmate::assertMatrix(K, any.missing = FALSE, mode = "numeric")
    checkmate::assertTRUE(nrow(K) == ncol(K))
    
    # check that ids are a subset of the columns
    checkmate::assertSubset(id_labelled, colnames(K))
    
    K <- K[, unique(id_labelled), drop = FALSE]
    Kn <- K - rowMeans(K)
    
    var_y*tcrossprod(Kn)
}

#' @description Function \code{get_mu_reference()} computes the 
#' reference expected values (one scalar value for each node/entity)
#' 
#' @name moments
#' @export
get_mu_reference <- function(K, id_labelled = colnames(K)) {
    get_mu(K = K, id_labelled = id_labelled, mu_y = 1)
}

#' @description Function \code{get_var_reference()} computes the 
#' reference variances (one scalar value for each node/entity), 
#' log10-transformed
#' 
#' @name moments
#' @importFrom stats var
#' @importFrom checkmate assertMatrix assertTRUE assertSubset 
#' @export
get_var_reference <- function(K, id_labelled = colnames(K)) {
    # check numeric, square, named matrix
    checkmate::assertMatrix(K, any.missing = FALSE, mode = "numeric")
    checkmate::assertTRUE(nrow(K) == ncol(K))
    
    # check that ids are a subset of the columns
    checkmate::assertSubset(id_labelled, colnames(K))
    
    K <- K[, unique(id_labelled), drop = FALSE]
    n <- ncol(K)
    
    log10(apply(K, 1, stats::var)*(n - 1))
}
