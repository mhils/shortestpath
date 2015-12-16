context("dijkstra")

test_that("dijkstra runs without errors", {
    graph <- randomGraph(n=10,euclidean=FALSE)
    r <- dijkstra(graph,"A","J")
    expect_is(r,"spresults")
})
