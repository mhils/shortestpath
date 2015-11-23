context("randomGraph")

test_that("randomGraph generates random graphs", {
    set.seed(1)
    g <- randomGraph()
    expect_equal(E(g)$weight, c(4, 6, 7, 1, 2, 1, 10, 7, 1, 5, 5, 4, 10, 2, 9))
})

test_that("randomGraph stops for invalid values of k", {
    expect_error(randomGraph(4,5), "The graph degree must not exceed #N-1")
    expect_error(randomGraph(4,1), "The graph degree k must be >=")
})

test_that("randomGraph works for large valid values of k", {
    expect_equal(length(E(randomGraph(4,3))), 6)
    expect_equal(length(E(randomGraph(4,2.5))), 5)
})

test_that("randomGraph works for small valid values of k", {
    expect_equal(length(E(randomGraph(10,1.8))), 9)
})

test_that("randomGraph does produce a graph with a single cluster", {
    set.seed(53569) # this was found by brute-force - turns out these cases are *very* rare!
    g <- randomGraph(8,2*7/8)
    expect_equal(no.clusters(g), 1)
})
