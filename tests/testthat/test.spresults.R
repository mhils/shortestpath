context("spresults")

test_that("the spresults constructor works as expected", {
    steps <- list(make_graph("Bull"))
    results <- spresults(steps)
    expect_true(is.spresults(results))
})

test_that("spresults$name gives the name of the last graph", {
    steps <- list(make_graph("Tetrahedral"), make_graph("Bull"))
    results <- spresults(steps)
    expect_equal(results$name, "Bull")
})

test_that("spresults$name<- throws an error", {
    steps <- list(make_graph("Bull"))
    results <- spresults(steps)
    expect_error(results$name <- "Bull", "Cannot modify spresults objects")
})

test_that("is.spresults is working as expected", {
    steps <- list(make_graph("Bull"))
    results <- spresults(steps)
    expect_true(is.spresults(results))
    expect_false(is.spresults(make_graph('Tetrahedral')))
    expect_false(is.spresults(NULL))
})
