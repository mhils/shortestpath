context("toLatexGraph")

test_that("toLatexGraph runs without errors", {
    graph <- randomGraph(n=20,euclidean=TRUE)
    a <- aStarSearch(graph,"A","K")
    fw <- floydWarshall(graph)
    a2 <- aStarSearch(as.directed(graph), "A", "K")

    expect_output(toLatexGraph(a),"tikz")
    expect_output(toLatexGraph(fw),"tikz")
    expect_output(toLatexGraph(a2),"tikz")
})

