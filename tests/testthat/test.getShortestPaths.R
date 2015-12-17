context("getShortestPaths")

test_that("getShortestPaths runs without errors", {
    graph <- randomGraph(n=20,euclidean=TRUE)
    a <- aStarSearch(graph,"A","K")
    b <- floydWarshall(graph)
    getShortestPaths(a)
    getShortestPaths(b,"A","K")
})

test_that("getShortestPaths handles incomplete instances", {
    graph <- as.spgraph(make_ring(5))
    a <- aStarSearch(graph,"A","B", function(...) 0)
    expect_error(getShortestPaths(a,"B","A"), "the given `from` parameter does not match the single source")
    expect_equal(getShortestPaths(a,"A","C"), list())
})

test_that("getShortestPaths returns valid results", {
    set.seed(20)
    graph <- randomGraph() %>%
        setVertexCoordinatesFromLayout() %>%
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
