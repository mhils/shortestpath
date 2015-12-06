context("plot.spgraph")

test_that("a shortest path graph plots without errors", {
    spgraph <- as.spgraph(make_graph("Dodecahedron"))
    plot(spgraph)
})

test_that("spresults are properly plotted", {
    spresults <- dijkstra(make_graph("Bull"), "A", "E")
    plot(spresults)
})

test_that("a meaningful error is raised if the graph argument is not an spgraph", {
    expect_error(plot.spgraph("illegal"))
})
