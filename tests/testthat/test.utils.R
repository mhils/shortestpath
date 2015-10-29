context("utils")

test_that("is.spgraph is working as expected", {
    expect_true(is.spgraph(randomGraph()))
    expect_false(is.spgraph(make_graph('Tetrahedral')))
    expect_false(is.spgraph(NULL))
})


test_that("euc.dist is working as expected", {
    g <- make_graph('Tetrahedral') %>%
        set_vertex_attr("x", value=1:4) %>%
        set_vertex_attr("y", value=rep(42,4))

    dists <- euc.dist(g, V(g)[1], V(g))
    expect_equal(dists, 0:3)
})

test_that("euc.dist stops if no coordinates are given", {
    g <- make_graph('Tetrahedral')
    expect_error(euc.dist(g, V(g)[1], V(g)))
})

test_that("get.vertex returns vertex objects as-is", {
    g <- make_graph('Tetrahedral')
    v <- V(g)[1]
    expect_equal(v, get.vertex(g,v))
})

test_that("get.vertex returns the first vertex for TRUE", {
    g <- make_graph('Tetrahedral')
    expect_equal(V(g)[1], get.vertex(g, TRUE))
})

test_that("get.vertex stops if identifier is invalid", {
    g <- make_graph('Tetrahedral')
    expect_error(get.vertex(g, "foo"))
})

test_that("get.vertex stops if more than one vertex is selected", {
    g <- make_graph('Tetrahedral')
    expect_error(get.vertex(g, nei(1)))
})

test_that("get.vertex correctly selects vertices by their id", {
    g <- make_graph('Tetrahedral') %>% setAlphabeticalVertexNames()
    expect_equal(V(g)[2], get.vertex(g, "B"))
})
