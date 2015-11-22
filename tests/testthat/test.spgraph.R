context("as.spgraph")

test_that("as.spgraph.igraph adds all required attributes", {
    spgraph <- as.spgraph(make_graph("Tetrahedral"))
    expect_true(is.spgraph(spgraph))
    expect_equal(vertex.attributes(spgraph)$name[1], "A")
    expect_equal(edge.attributes(spgraph)$weight[1], 1)
    dists <- matrix(Inf,ncol=4,nrow=4)
    diag(dists) <- 0
    colnames(dists) <- rownames(dists) <- c("A", "B", "C", "D")
    expect_equal(graph.attributes(spgraph)$min_dists, dists)
    expect_equal(graph.attributes(spgraph)$shortest_path_predecessors[[1, 1]], NULL)
    expect_equal(spgraph$from, FALSE)
    expect_equal(spgraph$to, FALSE)
})
