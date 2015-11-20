context("aStarSearch")

test_that("aStarSearch runs without errors", {

    graph <- randomGraph(n=20,euclidean=TRUE)
    par(mfrow=c(1,2))

    r <- aStarSearch(graph,"A","K")
    plot(r[[length(r)]])
    rDijsktra <- aStarSearch(graph,"A","K", distance.heuristic = function(g,v1,v2) 0 )
    plot(rDijsktra[[length(rDijsktra)]])
    par(mfrow=c(1,1))
})

test_that("aStarSearch finds the minimal distance", {

    test_configurations = list(
        list(n=2, k=1),
        list(n=4, k=2*3/4),
        list(n=4, k=2),
        list(n=4, k=3),
        list(n=20, k=2.5)
    )

    for(args in test_configurations){
        start <- "A"
        stop <- LETTERS[args$n]
        graph <- randomGraph(no.of.nodes=args$n, k=args$k, euclidean=TRUE)
        r <- aStarSearch(graph,start,stop)
        last_step <- r[[length(r)]]
        reference_value <- distances(graph, v=start, to=stop)[1,1]
        expect_equal(last_step$min_dists[stop,], reference_value)
    }
})

