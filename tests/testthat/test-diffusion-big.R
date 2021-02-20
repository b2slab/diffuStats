context("Diffusion functions")

library(igraph)
library(Matrix)
library(Rcpp)

set.seed(1)

# Small example
n <- 1e3
n.perm <- 1e3
n.cores <- 2
seed <- 1

# Directed graph
graph <- barabasi.game(n, directed = FALSE)
V(graph)$name <- paste0("A", 1:n)

# Random scores
scores <- list(
    bkgd1 = as(
        matrix(
            rbinom(10*n, size = 1, prob = .5),
            ncol = 20),
        "sparseMatrix")
)
rownames(scores$bkgd1) <- V(graph)[1:nrow(scores$bkgd1)]$name
colnames(scores$bkgd1) <- paste0("p", 1:ncol(scores$bkgd1))

# Second set of scores
# background with a different amount of nodes
scores$bkgd2 <- head(scores$bkgd1, nrow(scores$bkgd1)/2)

sample.prob = list(
    bkgd1 = (setNames(
        degree(graph)[1:(n/2)],
        1:(n/2))),
    bkgd2 = (setNames(
        degree(graph)[1:(n/4)],
        1:(n/4))))


test_that("'diffuse_mc' consistency check and agreement with z-scores", {
    # Apply main function
    expect_error({
        heatrank <- diffuse_mc(
            graph = graph,
            scores = scores,
            n.perm = n.perm,
            sample.prob = sample.prob,
            seed = seed,
            oneminusHeatRank = TRUE,
            K = NULL)
    }, NA)

    # Dimensions are as expected
    expect_equal(dim(heatrank$bkgd1), dim(heatrank$bkgd2))

    # heatrank lies between 0 and 1
    expect_true(all(heatrank$bkgd1 >= 0))
    expect_true(all(heatrank$bkgd2 >= 0))

    expect_true(all(heatrank$bkgd1 <= 1))
    expect_true(all(heatrank$bkgd2 <= 1))

    # small correction, heatrank should never be zero (and therefore
    # 1-heatrank should never be 1)
    expect_false(any(heatrank$bkgd1 == 1))
    
    # expect that, in general, source nodes have higher scores
    # because oneminusHeatRank = TRUE, best scores should be those of
    # best ranked nodes
    test.scores <- lapply(
        colnames(scores$bkgd1), 
        function(column) {
            # input positives and negatives
            pos <- scores$bkgd1[, column] == 1L
            neg <- scores$bkgd1[, column] == 0
            # are input positives better ranked than input negatives?
            wilcox.test(
                heatrank$bkgd1[pos, column], 
                heatrank$bkgd1[neg, column], 
                alternative = "greater"
            )
        }
    )
    test.pvals <- vapply(test.scores, function(x) x$p.value, FUN.VALUE = .1)
    # > range(test.pvals)
    # [1] 4.108775e-42 4.714910e-29
    expect_true(all(test.pvals < 1e-20))
    
    # expect that, in general, simulated scores correlate with parametric ones
    z <- diffuse_raw(
        graph = graph,
        scores = scores,
        z = TRUE,
        K = NULL)
    
    # compute linear correlation and p-value
    cor.scores <- lapply(
        colnames(scores$bkgd1), 
        function(column) {
            cor.test(heatrank$bkgd1[, column], z$bkgd1[, column])
        }
    )
    cor.estimates <- vapply(cor.scores, function(x) x$estimate, FUN.VALUE = .1)
    cor.pvals <- vapply(cor.scores, function(x) x$p.value, FUN.VALUE = .1)
    # > range(cor.estimates)
    # [1] 0.8489368 0.9228191
    expect_true(all(cor.estimates > .8))
    # > range(cor.pvals)
    # [1]  0.000000e+00 1.168082e-278
    expect_true(all(cor.pvals < 1e-100))
})


test_that("'diffuse_raw' consistency check", {
    # Apply main function
    expect_error({
        raw <- diffuse_raw(
            graph = graph,
            scores = scores,
            K = NULL)
    }, NA)

    # Dimensions are as expected
    expect_equal(dim(raw$bkgd1), dim(raw$bkgd2))

    # As defined, scores should be greater or equal to zero
    expect_true(all(raw$bkgd1 >= 0))
    expect_true(all(raw$bkgd2 >= 0))
})


test_that("'diffuse' consistency check", {
    methods_raw <- c("raw", "ml", "gm", "ber_s", "ber_p")
    # Try diffusion with several methods
    plyr::l_ply(
        setNames(methods_raw, methods_raw),
        function(method) {
            # Apply main function
            message(method)
            expect_error({
                final <- diffuse(
                    graph = graph,
                    scores = scores,
                    method = method,
                    n.perm = 1e2)
            }, NA)

            # Dimensions are as expected
            expect_equal(dim(final$bkgd1), dim(final$bkgd2))
            expect_equal(colnames(final$bkgd1), colnames(scores$bkgd1))
            expect_true(all(rownames(scores$bkgd1) %in% rownames(final$bkgd1)))
        }
    )
})

