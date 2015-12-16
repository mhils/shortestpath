context("toLatexGraph")

test_that("toLatexGraph runs without errors", {
    graph <- randomGraph(n=20,euclidean=TRUE)
    a <- aStarSearch(graph,"A","K")
    fw <- floydWarshall(graph)

    expect_output(toLatexGraph(a),"tikz")
    expect_output(toLatexGraph(fw),"tikz")
})

