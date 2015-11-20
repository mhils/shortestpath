context("makeShortestPathGraph")

test_that("makeShortestPathGraph adds all required attributes", {
    spgraph <- makeShortestPathGraph(make_graph("Tetrahedral"), "B", "D")
    expect_true(is.spgraph(spgraph))
    expect_equal(vertex.attributes(spgraph)$name[1], "A")
    expect_equal(edge.attributes(spgraph)$weight[1], 1)
    dists <- c(Inf, 0, Inf, Inf)
    names(dists) <- c("A", "B", "C", "D")
    expect_equal(graph.attributes(spgraph)$min_dists[, 1], dists)
    expect_equal(graph.attributes(spgraph)$shortest_path_predecessors[[1, 1]], NULL)
    expect_equal(spgraph$from, V(spgraph)["B"])
    expect_equal(spgraph$to, V(spgraph)["D"])
})
