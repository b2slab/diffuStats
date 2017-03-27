#' Diffuse scores on a network
#'
#' Function \code{diffuse} takes a network in
#' \code{\link[igraph]{igraph}} format and an initial state
#' to score all the nodes in the network.
#'
#' @param graph \code{\link[igraph]{igraph}} object for the diffusion
#' @param scores list of score matrices. For a single input with a
#' single background, supply a list with a vector column
#' @param diag.sum numeric constant or vector, amount
#' to add to regularise the Laplacian kernel if computed on the fly
#' @param z logical, should z-scores be computed instead of raw scores?
#' @param K optional matrix, precomputed diffusion kernel
#'
#' @return A list of scores, with the same length and
#' dimensions as \code{scores}
#'
#' @import igraph
#' @export
diffuse_raw <- function(
  graph,
  scores,
  diag.sum = 1,
  z = FALSE,
  K = NULL) {

  # Kernel matrix
  if (is.null(K)) {
    message("Matrix not supplied. Computing conductance...")
    L <- graph.laplacian(
      graph = graph,
      normalized = FALSE,
      sparse = TRUE)
    # Connect pathways to boundary
    Matrix::diag(L) <- Matrix::diag(L) + diag.sum

    message("Done")
    message("Computing inverse...")
    K <- as.matrix(Matrix::solve(L))
    rownames(K) <- colnames(K) <- V(graph)$name
    rm(L)
    gc()
    message("Done")
  } else {
    message("Using supplied kernel matrix...")
  }

  # Compute scores
  # browser()
  ans.all <- plyr::llply(
    stats::setNames(names(scores), names(scores)),
    function(scores.name) {
      # n <- nrow(R.whole)
      # scores.name <- "bkgd1"
      # browser()
      # match indices (NO NAMES, careful)
      bkgd.names <- rownames(scores[[scores.name]])
      input.names <- colnames(scores[[scores.name]])

      # input scores
      scores.mat <- methods::as(scores[[scores.name]], "sparseMatrix")

      # raw scores
      diff.raw <- K[, bkgd.names] %*% scores.mat

      # browser()
      rownames(diff.raw) <- rownames(K)
      colnames(diff.raw) <- input.names

      gc()
      # Return base matrix if it is raw
      if (z == FALSE) return(as.matrix(diff.raw))

      # If we want z-scores, must compute rowmeans and rowmeans2
      .rowSums <- Matrix::rowSums(K[, bkgd.names])
      .rowSums2 <- Matrix::rowSums(K[, bkgd.names] ** 2)

      # Constant terms over columns
      n <- length(bkgd.names)
      const_mean <- .rowSums/n
      const_var <-  (n*.rowSums2 - .rowSums**2)/((n - 1)*(n**2))

      # diff.z <- apply(
      #   diff.raw,
      #   2,
      #   function(col) {
      #     s1 <- sum(col)
      #     s2 <- sum(col**2)
      #
      #     # means and vars depend on first and second moments
      #     # of the input. This should be valid for non-binary
      #     # inputs as well
      #     score_means <- const_mean*s1
      #     score_vars <- const_var*(n*s2 - s1**2)
      #
      #     (col - score_means)/sqrt(score_vars)
      #   }
      # )
      diff.z <- sapply(
        1:ncol(diff.raw),
        function(col_ind) {
          col_in <- scores.mat[, col_ind]
          col_raw <- diff.raw[, col_ind]

          s1 <- sum(col_in)
          s2 <- sum(col_in**2)

          # means and vars depend on first and second moments
          # of the input. This should be valid for non-binary
          # inputs as well
          score_means <- const_mean*s1
          score_vars <- const_var*(n*s2 - s1**2)

          (col_raw - score_means)/sqrt(score_vars)
        }
      )
      # Give correct names
      dimnames(diff.z) <- dimnames(diff.raw)

      # browser()
      # if (any(is.na(diff.z))) browser()
      #
      # This is already a base matrix
      return(diff.z)
    }
  )

  return(ans.all)


    # temp.means.whole <- rowSums.OBS*s1.OBS/n.pool.OBS
    # temp.vars.OBS <- (n.pool.OBS*s2.OBS - s1.OBS^2)/
    # (n.pool.OBS*(n.pool.OBS - 1))*(rowSquaredSums.OBS - (rowSums.OBS^2)/
    # n.pool.OBS)















  # message("Checking scores and backgrounds...")
  #
  # # scorenames.length <- lapply(scores, length)
  # # if (length(table(unlist(scorenames.length))) > 1)
  # stop("Score vectors differ on length...")
  # #
  # # allnames.scores <- table(sapply(scores, names))
  # # if (any(allnames.scores != length(scores)))
  # stop("Different names between score vectors...")
  # #
  # # names.scores <- names(scores[[1]])
  # # scores <- lapply(scores, function(x) x[[names.scores]])
  #
  # # id.pool.whole <- intersect(names.scores, V(graph.whole)$name)
  #
  # # Where is the flow drain?
  # g_boundary <- get.graph.attribute(g, "drain")
  # if (is.null(g_boundary)) {
  #   # by default, the nodes outside the pool absorbe heat
  #   id.boun.whole <- setdiff(V(graph.whole)$name, id.pool.whole)
  #   if (length(id.boun.whole) == 0) stop("Default pool would be empty.
  #   Please specify one...")
  # } else {
  #   id.boun.whole <- intersect(V(graph.whole)$name, id.boundary)
  # }
  #
  # message("Done")
  #
  # if (is.null(path.kernel)) {
  #   message("Kernel not supplied. Computing it...")
  #   KI.whole <- graph.laplacian(
  #     graph = graph.whole,
  #     normalized = F,
  #     sparse = F)
  #   # Connect pathways to boundary
  #   diag(KI.whole)[id.boun.whole] <- diag(KI.whole)[id.boun.whole] + 1
  #
  #   message("Done")
  #   message("Computing inverse...")
  #   R.whole <- solve(KI.whole, sparse = F)
  #   rm(KI.whole)
  #   gc()
  #   message("Done")
  # } else {
  #   message("Loading supplied matrix...")
  #   load(path.matrix)
  #   message("Done")
  # }
  #
  # # Uncomment to save the matrix
  # # browser()
  #
  # rowSums.OBS <- rowSums(R.whole[, id.pool.OBS])
  # rowSquaredSums.OBS <- rowSums(R.whole[, id.pool.OBS] ^ 2)
  # rowSums.LAT <- rowSums(R.whole[, id.pool.LAT])
  # rowSquaredSums.LAT <- rowSums(R.whole[, id.pool.LAT] ^ 2)
  #
  # # s1 and s2 from scores
  # message("Computing answers for scorelist...")
  # ans.all <- lapply(scores, function(score.particular) {
  #   score.particular <- score.particular[id.pool.whole]
  #
  #   s1.OBS <- sum(score.particular)
  #   s2.OBS <- sum(score.particular ^ 2)
  #   s1.LAT <- s1.OBS*n.pool.LAT/n.pool.OBS
  #   s2.LAT <- s2.OBS*n.pool.LAT/n.pool.OBS
  #
  #   temp.means.whole <- rowSums.OBS*s1.OBS/n.pool.OBS
  #   temp.vars.OBS <- (n.pool.OBS*s2.OBS - s1.OBS^2)/
  #   (n.pool.OBS*(n.pool.OBS - 1))*(rowSquaredSums.OBS - (rowSums.OBS^2)/
  #   n.pool.OBS)
  #   temp.vars.LAT <- (n.pool.LAT*s2.LAT - s1.LAT^2)/
  #   (n.pool.LAT*(n.pool.LAT - 1))*(rowSquaredSums.LAT - (rowSums.LAT^2)/
  #   n.pool.LAT)
  #
  #   temp.vars.whole <- temp.vars.OBS + 2*temp.vars.LAT
  #   temp.vars.PROPORTION <- temp.vars.OBS/temp.vars.whole
  #
  #   scores.included <- numeric(ncol(R.whole))
  #   names(scores.included) <- colnames(R.whole)
  #   scores.included[names(score.particular)] <- score.particular
  #
  #   temp.final.whole <- as.numeric(R.whole %*% scores.included)
  #   names(temp.final.whole) <- rownames(R.whole)
  #
  #   #p-vals for the WHOLE OBS model
  #   my.score.OBS <- (temp.final.whole - temp.means.whole)/sqrt(temp.vars.OBS)
  #   #     my.pvals.OBS <- pnorm(q = my.score.OBS,
  #   #                           lower.tail = F)
  #   #     my.fdr.OBS <- p.adjust(my.pvals.OBS, method = p.adjust)
  #   #     my.fdr.sig.OBS <- names(my.fdr.OBS)[my.fdr.OBS < p.threshold]
  #
  #   #p-vals for the WHOLE latent model
  #   if (!is.null(id.pool.LAT)) {
  #     my.score.whole <- (temp.final.whole - temp.means.whole)/
  #
  #     sqrt(temp.vars.whole)
  #     #       my.pvals.whole <- pnorm(q = my.score.whole,
  #     #                               lower.tail = F)
  #     #       my.fdr.whole <- p.adjust(my.pvals.whole, method = p.adjust)
  #     #       my.fdr.sig.whole <- names(my.fdr.whole)[my.fdr.whole <
  #     p.threshold]
  #   }
  #
  #   # Return the interesting results...
  #   ans <- list(OBS = list(), LAT = list(), PROPORTION = numeric())
  #
  #   ans$OBS$zscore <- my.score.OBS
  #   ans$OBS$temperature <- temp.final.whole
  #   # ans$OBS$p.raw <- my.pvals.OBS
  #   # ans$OBS$p.adj <- my.fdr.OBS
  #   # ans$OBS$sig <- my.fdr.sig.OBS
  #
  #   if (!is.null(id.pool.LAT)) {
  #     ans$LAT$zscore <- my.score.whole
  #     #       ans$LAT$p.raw <- my.pvals.whole
  #     #       ans$LAT$p.adj <- my.fdr.whole
  #     #       ans$LAT$sig <- my.fdr.sig.whole
  #
  #     ans$PROPORTION <- temp.vars.PROPORTION
  #   } else {
  #     ans$LAT <- NULL
  #     ans$PROPORTION <- NULL
  #   }
  #
  #   return(ans)
  # })
  #
  # names(ans.all) <- names(scores)
  # message("Done")
  #
  # return(ans.all)
}
