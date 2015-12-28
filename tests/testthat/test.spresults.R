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

test_that("spresults$first/spresults$last give the first/last graph", {
    a <- as.spgraph(make_graph("Tetrahedral"))
    b <- as.spgraph(make_graph("Bull"))
    steps <- list(a,b)
    results <- spresults(steps)
    expect_equal(results$first, a)
    expect_equal(results$last, b)
})

test_that("is.spresults is working as expected", {
    steps <- list(as.spgraph(make_graph("Bull")))
    results <- spresults(steps)
    expect_true(is.spresults(results))
    expect_false(is.spresults(make_graph('Tetrahedral')))
    expect_false(is.spresults(NULL))
})

test_that("print.spresults is working as expected", {
    d <- dijkstra(make_graph("Bull"), "A", "E")
    expect_output(print(d), "A->E")
    fw <- floydWarshall(make_graph("Bull"))
    expect_output(print(fw), "all shortest paths")
})

test_that("print.spresults displays the shortest path", {
    g <- make_ring(4) %>% set_edge_attr("weight", value=1:4)
    d <- dijkstra(g, "A", "C")
    expect_output(print(d), "path: A->B->C")

    g %<>% set_edge_attr("weight",value=1)
    d <- dijkstra(g, "A", "C")
    expect_output(print(d), "(+1 alternatives)")

    g %<>% delete_edges(1:4)
    d <- dijkstra(g, "A", "C")
    expect_output(print(d), "(no path found)")
})
