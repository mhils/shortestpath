context("export")

graph <- randomGraph(n=20,euclidean=TRUE)
a <- aStarSearch(graph,"A","K")
fw <- floydWarshall(graph)

test_that("toLatexTable runs without errors", {
    expect_output(toLatexTable(a),"multicolumn")
    expect_output(toLatexTable(fw),"multicolumn")
})

