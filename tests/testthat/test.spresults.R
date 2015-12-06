context("spresults")

test_that("the spresults constructor works as expected", {
    steps <- list(as.spgraph(make_graph("Bull")))
    results <- spresults(steps)
    expect_true(is.spresults(results))
})

test_that("the spresults constructor does not accept graphs which arent's spgraphs", {
    steps <- list(make_graph("Bull"))
    expect_error(spresults(steps), "not an spgraph")
})

test_that("spresults$name gives the name of the last graph", {
    steps <- list(
        as.spgraph(make_graph("Tetrahedral")),
        as.spgraph(make_graph("Bull")))
    results <- spresults(steps)
    expect_equal(results$name, "Bull")
})

test_that("spresults$name<- throws an error", {
    steps <- list(as.spgraph(make_graph("Bull")))
    results <- spresults(steps)
    expect_error(results$name <- "Bull", "Cannot modify spresults objects")
})

test_that("is.spresults is working as expected", {
    steps <- list(as.spgraph(make_graph("Bull")))
    results <- spresults(steps)
    expect_true(is.spresults(results))
    expect_false(is.spresults(make_graph('Tetrahedral')))
    expect_false(is.spresults(NULL))
})

test_that("print is overwritten", {
    results <- dijkstra(make_graph("Bull"), "A", "E")
    expect_output(print(results), "min dist: 2")
})
