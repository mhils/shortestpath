context("plot.spgraph")

test_that("a shortest path graph plots without errors", {
  plot(makeShortestPathGraph(karate))
})

test_that("a meaningful error is raised if the graph argument is not an spgraph", {
  expect_error(plot.spgraph("illegal"))
})