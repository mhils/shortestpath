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

test_that("setSingleSource works as expected", {
    g <- make_graph('Tetrahedral') %>%
        setAlphabeticalVertexNames() %>%
        setInfiniteMinDists() %>%
        setEmptyShortestPathPredecessors() %>%
        setSingleSource("B")
    expect_equal(rownames(g$min_dists), LETTERS[1:4])
    expect_equal(colnames(g$min_dists), c("B"))
})

test_that("setUniformVertexSets works as expected", {
    g <- make_graph('Dodecahedron') %>%
        setUniformVertexSets(val="front")
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
