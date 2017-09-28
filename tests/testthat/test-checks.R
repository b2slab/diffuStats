context("Test if checks give proper errors and warnings")

mat_na <- matrix(0)

scores1 <- list(bkgd1 = matrix(0))
test_that("Scores testing", {
    # list with no names
    expect_error(
        .check_scores(list(mat_na)),
        regexp = "named list"
    )
    # data with NA
    mat_na0 <- matrix(NA_real_)
    expect_error(
        .check_scores(to_list(mat_na0)),
        regexp = "NA"
    )
    # no rownames/colnames
    expect_error(
        .check_scores(to_list(mat_na)),
        regexp = "rownames"
    )

    # no colnames
    rownames(mat_na) <- "test"
    expect_error(
        .check_scores(to_list(mat_na)),
        regexp = "colnames"
    )

    # std is NA
    colnames(mat_na) <- "test"
    expect_warning(
        .check_scores(to_list(mat_na)),
        regexp = "Standard deviation .+ is NA .+"
    )

    # std is zero in column 2
    mat_na2 <- matrix(c(0, 1, 0, 0), nrow = 2)
    colnames(mat_na2) <- rownames(mat_na2) <- paste0("test", 1:2)
    expect_warning(
        .check_scores(to_list(mat_na2)),
        regexp = "Standard deviation .+ is 0 .+2"
    )

    # proper format passes test
    data("graph_toy")
    expect_error(
        to_list(graph_toy$input_mat),
        NA
    )
})

test_that("Method testing", {
    expect_error(
        .check_method(list(a = 42)),
        regexp = "must be a character"
    )
    expect_error(
        .check_method(c("gm", "ml")),
        regexp = "Only one 'method'"
    )

    expect_error(
        .check_method(NA_character_),
        regexp = "available methods .+ 'NA'"
    )
    expect_error(
        .check_method("hello"),
        regexp = "available methods"
    )

    # implemented methods pass the test
    plyr::l_ply(
        .available_methods,
        function(method) {
            expect_error(
                .check_method(method),
                NA
            )
        }
    )
})

test_that("Metric testing", {
    # no list
    expect_error(
        .check_metric(1:10),
        regexp = "must be a list"
    )
    # list with no names
    expect_error(
        .check_metric(list(1)),
        regexp = "names"
    )
    # list of other things
    expect_error(
        .check_metric(list(a = 42)),
        regexp = "function"
    )
    # functions with only one argument
    f1 <- function(x) x
    expect_error(
        .check_metric(list(f1 = f1)),
        regexp = "argument"
    )
    # warning: functions with more than two arguments
    f2 <- function(x, y, z) x*y
    expect_warning(
        .check_metric(list(f2 = f2)),
        regexp = "argument"
    )
    # warning: functions that return non-scalar stuff
    # e.g. vector of length 2
    f3 <- function(x, y) c(1:2)
    expect_warning(
        .check_metric(list(f3 = f3)),
        regexp = "length greater"
    )

    # flawless execution
    expect_error(
        .check_metric(list(
            auc = metric_fun(curve = "ROC"),
            pauc = metric_fun(curve = "ROC", partial = c(0, 0.1)),
            prc = metric_fun(curve = "PRC")
        )),
        NA
    )
})

test_that("Graph testing", {
    # no graph
    expect_error(
        .check_graph(1:10),
        regexp = "must be an igraph"
    )
    # graph with no names
    g <- barabasi.game(10, directed = FALSE)
    expect_error(
        .check_graph(g),
        regexp = "must have node names"
    )
    # NA names
    V(g)$name <- c(rep("a", vcount(g) - 1), NA)
    expect_error(
        .check_graph(g),
        regexp = "NA"
    )
    # repeated names
    V(g)$name <- rep("a", vcount(g))
    expect_error(
        .check_graph(g),
        regexp = "unique"
    )
    # repeated names
    V(g)$name <- rep("a", vcount(g))
    expect_error(
        .check_graph(g),
        regexp = "unique"
    )
    # directed graph
    V(g)$name <- paste0("a", 1:vcount(g))
    g <- as.directed(g)
    expect_warning(
        .check_graph(g),
        regexp = "undirected"
    )
    # flawless
    g <- as.undirected(g)
    expect_error(
        .check_graph(g),
        NA
    )
    # NA edge weights
    E(g)$weight <- 1
    E(g)[1]$weight <- NA
    expect_error(
        .check_graph(g),
        regexp = "NA"
    )
    # negative
    E(g)[1]$weight <- -1
    expect_warning(
        .check_graph(g),
        regexp = "negative"
    )
    # flawless
    E(g)[1]$weight <- 42
    expect_error(
        .check_graph(g),
        NA
    )
})

test_that("Kernel testing (no spd check)", {
    # wrong type
    expect_error(
        .check_K(1:10),
        "must be a matrix"
    )
    mat_play <- matrix("hello")
    rownames(mat_play) <- colnames(mat_play) <- "test"
    expect_error(
        .check_K(mat_play),
        "numeric"
    )
    mat_play <- matrix(NA_real_)
    rownames(mat_play) <- colnames(mat_play) <- "test"
    expect_error(
        .check_K(mat_play),
        "NA"
    )
    # must be square
    mat_play <- matrix(1:10, nrow = 5)
    rownames(mat_play) <- paste0("r", 1:nrow(mat_play))
    colnames(mat_play) <- paste0("c", 1:ncol(mat_play))
    expect_error(
        .check_K(mat_play),
        "square"
    )
    # need dimnames
    mat_play <- matrix(1:25, nrow = 5)
    rownames(mat_play) <- paste0("r", 1:nrow(mat_play))
    expect_error(
        .check_K(mat_play),
        "colnames"
    )
    rownames(mat_play) <- NULL
    colnames(mat_play) <- paste0("c", 1:ncol(mat_play))
    expect_error(
        .check_K(mat_play),
        "rownames"
    )
    #dimnames cannot be NA
    rownames(mat_play) <- paste0("r", 1:nrow(mat_play))
    rownames(mat_play)[1] <- NA
    expect_error(
        .check_K(mat_play),
        "NA"
    )
    # dimnames must coincide
    rownames(mat_play) <- paste0("r", 1:nrow(mat_play))
    expect_error(
        .check_K(mat_play),
        "coincide"
    )
    # flawless
    rownames(mat_play) <- paste0("c", 1:nrow(mat_play))
    expect_error(
        .check_K(mat_play),
        NA
    )
    # repeated names must raise error
    rownames(mat_play) <- colnames(mat_play) <- rep("a", nrow(mat_play))
    expect_error(
        .check_K(mat_play),
        "duplicated"
    )
})
