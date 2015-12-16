context("dijkstra")

test_that("dijkstra runs without errors", {
    graph <- randomGraph(n=10,euclidean=FALSE)
    dijkstra(graph,"A","J")
})
