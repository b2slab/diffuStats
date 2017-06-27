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
graph <- barabasi.game(n, directed = F)
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


test_that("'diffuse_mc' consistency check", {
    # Apply main function
    expect_error({
        heatrank <- diffuse_mc(
            graph = graph,
            scores = scores,
            n.perm = n.perm,
            sample.prob = sample.prob,
            seed = seed,
            oneminusHeatRank = FALSE,
            K = NULL)
    }, NA)

    # Dimensions are as expected
    expect_equal(dim(heatrank$bkgd1), dim(heatrank$bkgd2))

    # heatrank lies between 0 and 1
    expect_true(all(heatrank$bkgd1 >= 0))
    expect_true(all(heatrank$bkgd2 >= 0))

    expect_true(all(heatrank$bkgd1 <= 1))
    expect_true(all(heatrank$bkgd2 <= 1))

    # small correction, heatrank should never be zero
    expect_false(any(heatrank$bkgd1 == 0))
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

