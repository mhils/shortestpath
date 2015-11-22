context("randomGraph")

test_that("randomGraph generates random graphs", {
    a <- randomGraph()
    for(i in 1:100){
        b <- randomGraph()
        if(edge_attr(a)$weight != edge_attr(b)$weight){
            expect_true(TRUE)
            break
        }
    }
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
