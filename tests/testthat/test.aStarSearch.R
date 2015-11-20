context("aStarSearch")

test_that("aStarSearch runs without errors", {


    graph <- randomGraph(n=20,euclidean=TRUE)
    par(mfrow=c(1,2))

    r <- aStarSearch(graph,"A","K")
    plot(r[[length(r)]])
    rDijsktra <- aStarSearch(graph,"A","K", distance.heuristic = function(g,v1,v2) 0 )
    plot(rDijsktra[[length(rDijsktra)]])

    # graph <- r[[4]]
    # plot(r[[1]])
    # plot(r[[2]])
    # plot(r[[3]])
    # plot(r[[4]])
    # plot(r[[5]])
    # plot(r[[6]])
    # plot(r[[7]])
    # plot(r[[8]])
    # plot(r[[9]])
    # plot(r[[10]])
    # plot(r[[11]])

})

