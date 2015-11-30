context("getShortestPaths")

test_that("getShortestPaths runs without errors", {
    graph <- randomGraph(n=20,euclidean=TRUE)
    a <- aStarSearch(graph,"A","K")
    b <- floydWarshall(graph)
    getShortestPaths(a)
    getShortestPaths(b,"A","K")
})

test_that("getShortestPaths returns valid results", {
    set.seed(20)
    graph <- randomGraph() %>%
        setVertexCoordinatesFromLayout(layout.fruchterman.reingold, list(niter=10000)) %>%
        setEuclideanEdgeWeights()

    #?aStarSearch
    result <- aStarSearch(graph,"H","F")
    r <- getShortestPaths(result)
    ref <- list(
        list(
            vertices=V(graph)[c("H","E","I","F")],
            edges=E(graph)["H"%--%"E", "E"%--%"I", "I"%--%"F"]
        ),
        list(
            vertices=V(graph)[c("H","E","B","F")],
            edges=E(graph)["H"%--%"E", "E"%--%"B", "B"%--%"F"]
        ),
        list(
            vertices=V(graph)[c("H","K","B","F")],
            edges=E(graph)["H"%--%"K", "K"%--%"B", "B"%--%"F"]
        )
    )
    expect_equal(r, ref)
})
