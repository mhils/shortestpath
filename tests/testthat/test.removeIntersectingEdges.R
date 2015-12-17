context("removeIntersectingEdges")

test_that("removeIntersectingEdges runs without errors", {
    r <- make_graph("Cubical") %>%
        as.spgraph() %>%
        removeIntersectingEdges()
    expect_is(r,"spgraph")
})

test_that("addNonIntersectingEdge gives up at some point", {
    r <- make_graph("HouseX") %>%
        as.spgraph()
    expect_warning(addNonIntersectingEdge(r), "Unable to add nonoverlapping edge")
})

test_that("removeIntersectingEdges does not create new clusters", {
    r <- make_ring(4) %>%
        delete_edges(4) %>%
        set_vertex_attr("x",value=c(0,1,1,0)) %>%
        set_vertex_attr("y",value=c(1,0,1,0))
    expect_warning(removeIntersectingEdges(r), "Cannot remove edge without creating a second cluster")
})
