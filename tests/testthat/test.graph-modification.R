context("graph modification")

test_that("setInfiniteMinDists works as expected", {
    g <- make_graph('Tetrahedral') %>%
        setInfiniteMinDists()
    expect_equal(
        g$min_dists,
        matrix(
            c( 0, Inf, Inf, Inf,
               Inf, 0, Inf, Inf,
               Inf, Inf, 0, Inf,
               Inf, Inf, Inf, 0 ),
            ncol=4
        )
    )
})

test_that("setRandomVertexCoordinates works as expected", {
    set.seed(1)
    g <- make_graph('Tetrahedral') %>%
        setRandomVertexCoordinates()
    expect_true(max(V(g)$x-c(2.655087, 3.721239, 5.728534, 9.082078)) < 1e-5)
    expect_true(max(V(g)$y-c(2.016819, 8.983897, 9.446753, 6.607978)) < 1e-5)
})

test_that("setVertexCoordinatesFromLayout works as expected", {
    set.seed(1)
    g <- make_graph('Tetrahedral') %>%
        setVertexCoordinatesFromLayout(layout=function(...) matrix(1:8, ncol=2))
    expect_equal(V(g)$x, 1:4)
    expect_equal(V(g)$y, 5:8)
})

test_that("setEuclideanEdgeWeights works as expected", {
    set.seed(1)
    g <- make_graph('Tetrahedral') %>%
        setVertexCoordinatesFromLayout() %>%
        setEuclideanEdgeWeights()
    expect_equal(E(g)$weight, c(1,2,1,1,1,2))
})

test_that("setEuclideanEdgeWeights stops if no vertex coordinates are given", {
    g <- make_graph('Tetrahedral')
    expect_error(setEuclideanEdgeWeights(g), "Cannot compute edge weights for graph without x,y coordinates.")
})

test_that("setRandomEdgeWeights works as expected", {
    g <- make_graph('Dodecahedron') %>%
        setRandomEdgeWeights()
    expect_equal(length(edge.attributes(g)$weight), 30)
})

test_that("setUniformEdgeWeights works as expected", {
    g <- make_graph('Dodecahedron') %>%
        setUniformEdgeWeights()
    expect_equal(edge.attributes(g)$weight, rep(1,30))
})

test_that("setEmptyShortestPathPredecessors works as expected", {
    g <- make_graph('Tetrahedral') %>%
        setEmptyShortestPathPredecessors()
    expect_equal(g$shortest_path_predecessors, matrix(list(), ncol=4, nrow=4))
})

test_that("setRoute works as expected", {
    g <- make_graph('Tetrahedral') %>%
        setAlphabeticalVertexNames() %>%
        setInfiniteMinDists() %>%
        setEmptyShortestPathPredecessors() %>%
        setRoute("B", "D")
    expect_equal(rownames(g$min_dists), LETTERS[1:4])
    expect_equal(colnames(g$min_dists), c("B"))
})

test_that("setVertexSets works as expected", {
    g <- make_graph('Dodecahedron') %>%
        setVertexSets(val="front")
    expect_equal(vertex.attributes(g)$set, rep("front", 20))
})

test_that("setAlphabeticalVertexNames works as expected", {
    g <- make_graph('Tetrahedral') %>%
        setAlphabeticalVertexNames()
    expect_equal(vertex.attributes(g)$name, LETTERS[1:4])
})

test_that("setAlphabeticalVertexNames handles graphs with >26 vertices", {
    g <- make_graph('Tutte') %>%
        setAlphabeticalVertexNames()
    expect_equal(vertex.attributes(g)$name, 1:46)
})
