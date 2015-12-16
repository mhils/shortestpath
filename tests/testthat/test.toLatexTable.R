context("toLatexTable")

test_that("toLatexTable runs without errors", {
    graph <- randomGraph(n=20,euclidean=TRUE)
    a <- aStarSearch(graph,"A","K")
    fw <- floydWarshall(graph)

    expect_output(toLatexTable(a),"multicolumn")
    expect_output(toLatexTable(fw),"multicolumn")
})

