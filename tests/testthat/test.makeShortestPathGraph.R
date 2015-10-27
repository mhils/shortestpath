context("makeShortestPathGraph")

#
# test_that("makeEdgeWeights is a valid default factory", {
#   edge.weights <- makeEdgeWeights(make_graph("Dodecahedron"))
#   expect_equal(edge.weights, rep(1,30))
# })
#
# test_that("makeVertexNames is a valid default factory", {
#   vertex.names <- makeVertexNames(make_graph("bull"))
#   expect_equal(vertex.names, c("A","B","C","D","E"))
# })
# test_that("makeVertexNames handles graphs with 27+ vertices", {
#   vertex.names <- makeVertexNames(make_graph("Tutte"))
#   expect_equal(vertex.names, 1:46)
# })
#
# test_that("makeVertexFronts is a valid default factory", {
#   vertex.front <- makeVertexFronts(make_graph("Dodecahedron"))
#   expect_equal(vertex.front, rep(NA,20))
# })
#
# test_that("makeGraphMinDists is a valid default factory", {
#   graph.min_dists <- makeGraphMinDists(make_graph("Tetrahedral"))
#   expect_equal(graph.min_dists, matrix(c(
#     0, Inf, Inf, Inf,
#     Inf, 0, Inf, Inf,
#     Inf, Inf, 0, Inf,
#     Inf, Inf, Inf, 0
#   ), ncol=4))
# })
#
# test_that("makeGraphShortestPathPredecessors is a valid default factory", {
#   graph.shortest_path_predecessors <- makeGraphShortestPathPredecessors(make_graph("Tetrahedral"))
#   expect_equal(graph.shortest_path_predecessors, matrix(c(NA), ncol=4, nrow=4))
# })


test_that("makeShortestPathGraph adds all required attributes", {
  spgraph <- makeShortestPathGraph(make_graph("Tetrahedral"), singleSource = "B")
  expect_true(is.spgraph(spgraph))
  expect_equal(vertex.attributes(spgraph)$name[1], "A")
  expect_equal(edge.attributes(spgraph)$weight[1], 1)
  expect_equal(vertex.attributes(spgraph)$set[1], NA)
  dists <- c(Inf,0,Inf,Inf)
  names(dists) <- c("A","B","C","D")
  expect_equal(graph.attributes(spgraph)$min_dists[,1], dists)
  expect_equal(graph.attributes(spgraph)$shortest_path_predecessors[1,1], NA)
  expect_equal(ncol(graph.attributes(spgraph)$min_dists), 1)
})
