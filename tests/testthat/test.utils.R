context("utils")

test_that("euclidean.vertex.distance is working as expected", {
    g <- make_graph('Tetrahedral') %>%
        set_vertex_attr("x", value=1:4) %>%
        set_vertex_attr("y", value=rep(42,4))

    dists <- euclidean.vertex.distance(g, V(g)[1], V(g))
    expect_equal(dists, 0:3)
})

test_that("euclidean.vertex.distance stops if no coordinates are given", {
    g <- make_graph('Tetrahedral')
    expect_error(euclidean.vertex.distance(g, V(g)[1], V(g)))
})

test_that("has.vertex.coordinates works as expected", {
    g <- make_graph('Tetrahedral')
    expect_false(has.vertex.coordinates(g))
    g %<>%
        set_vertex_attr("x", value=1:4) %>%
        set_vertex_attr("y", value=rep(42,4))
    expect_true(has.vertex.coordinates(g))
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
    expect_error(get.vertex(g, c(1,2)), "Identifier '1 2' selected more than one vertex: 1 2")
})

test_that("get.vertex stops if no vertex is selected", {
    g <- make_graph('Tetrahedral')
    expect_error(get.vertex(g, c()), "Identifier '' selected no vertices")
})

test_that("get.vertex correctly selects vertices by their id", {
    g <- make_graph('Tetrahedral') %>% setAlphabeticalVertexNames()
    expect_equal(V(g)[2], get.vertex(g, "B"))
})

test_that("is_edge_intersection correctly determines intersections", {
    set.seed(1)
    g <- as.spgraph(make_graph("HouseX"))
    # e1 and e2 intersect
    e1 <- E(g)["A" %--% "C"]
    e2 <- E(g)["B" %--% "D"]
    e3 <- E(g)["D" %--% "E"]

    expect_true(is_edge_intersection(g, c(e1, e2)))
    expect_false(is_edge_intersection(g, c(e1, e3)))
    expect_false(is_edge_intersection(g, c(e2, e3)))
})

test_that("is_edge_intersection detects direct overlaps", {
    g <- as.spgraph(make_graph("HouseX"))
    e1 <- E(g)[1]
    expect_true(is_edge_intersection(g, c(e1, e1)))

    expect_true(is_edge_intersection(g, matrix(ncol=2, c(
        V(g)[1],V(g)[2],
        V(g)[2],V(g)[1]))
    )
    )
})

test_that("is_edge_intersection works for vertex matrices", {
    g <- as.spgraph(make_graph("HouseX"))
    m <- matrix(c("A","C","B","D"), ncol=2, byrow=T)
    expect_true(is_edge_intersection(g, m))
})

test_that("is_edge_intersection fails without vertex  coordinates", {
    g <- make_graph("HouseX")
    e1 <- E(g)[1]
    expect_error(is_edge_intersection(g, c(e1, e1)), "without vertex coordinates")
})

test_that("is_edge_intersection fails for invalid edges_or_vertices", {
    g <- as.spgraph(make_graph("HouseX"))
    expect_error(is_edge_intersection(g, NULL), "Invalid edges_or_vertices")
})
