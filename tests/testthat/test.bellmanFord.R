context("bellmanFord")

test_that("bellmanFord runs without errors", {
    graph <- randomGraph(n = 20, euclidean = TRUE)
    bellmanFord(graph,"A","K")
})

test_that("bellmanFord finds the minimal distance", {

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
        bF <- bellmanFord(graph,start,stop)
        reference_value <- distances(graph, v=start, to=stop)[1,1]
        expect_equal(bF$min_dists[1,stop], reference_value)
    }
})

test_that("bellmanFord produces valid shortest_path_predecessors", {

    adj <- matrix(
        c(0,1,1,0,0,
          1,0,0,2,0,
          1,0,0,2,0,
          0,2,2,0,4,
          0,0,0,4,0
        ), ncol=5)
    graph <- graph_from_adjacency_matrix(adj, weighted=T)

    A <- 1 #V(graph)[[1]]
    B <- 2 #V(graph)[[2]]
    C <- 3 #V(graph)[[3]]
    D <- 4 #V(graph)[[4]]
    E <- 5 #V(graph)[[5]]
    spp = matrix(list(
        NULL,A,A,c(B,C),D)
        ,ncol = 5, byrow = T
        )
    colnames(spp) = LETTERS[1:5]
    rownames(spp) = "A"
    results <- bellmanFord(graph,1,5)

    expect_equal(spp, results$shortest_path_predecessors)
})

test_that("bellmanFord detects negative cycles", {

    adj <- matrix(
        c(
         #A B C D E
          0,1,0,0,0, #A
          1,0,-2,4,0, #B
          0,-2,0,-3,0, #C
          0,4,-3,0,1, #D
          0,0,0,1,0  #E
        ), ncol=5)
    graph <- graph_from_adjacency_matrix(adj, weighted=T)
    expect_error(bellmanFord(graph,1,5),"graph has a negative cycle")
})

test_that("bellmanFord handles graphs without edges", {
    graph <- make_ring(3) %>% delete_edges(1:3)
    expect_is(bellmanFord(graph,"A","C"), "spresults")
})
