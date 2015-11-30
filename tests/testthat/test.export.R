context("export")

graph <- randomGraph(n=20,euclidean=TRUE)
a <- aStarSearch(graph,"A","K")
fw <- floydWarshall(graph)

test_that("toLatexTable runs without errors", {
    toLatexTable(a)
    toLatexTable(fw)
})

