context("plot.spgraph")

test_that("a shortest path graph plots without errors", {
    spgraph <- as.spgraph(make_graph("Dodecahedron"))
    plot(spgraph)
})

test_that("spresults are properly plotted", {
    spresults <- dijkstra(make_graph("Bull"), "A", "E")
    plot(spresults)
})

test_that("manually specifying attributes works", {
    spgraph <- as.spgraph(make_graph("Dodecahedron"))
    plot(
        spgraph,
        vertex.color.by="manual",
        vertex.color="#FAADBB",
        vertex.frame.color.by="manual",
        vertex.frame.color="#5E3443",
        edge.color.by="manual",
        edge.color = "#DE7192",
        edge.label.color = "#D96788",
        vertex.label.color	= "#DE7192"
    )
})

test_that("a meaningful error is raised if the graph argument is not an spgraph", {
    expect_error(plot.spgraph("illegal"))
})
