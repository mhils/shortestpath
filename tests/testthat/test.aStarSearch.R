context("aStarSearch")

test_that("aStarSearch runs without errors", {


    graph <- randomGraph(euclidean=TRUE)
    par(mfrow=c(1,1))
    plot(graph)

    r <- aStarSearch(graph,"A","G")
    par(mfrow=c(3,2))
    for(p in r){
        plot(p)
    }

})
