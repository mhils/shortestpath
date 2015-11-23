context("floydWarshall")

test_that("floydWarshall runs without errors", {
    graph <- randomGraph(n = 20, euclidean = TRUE)
    floydWarshall(graph)
})

test_that("floydWarshall finds the minimal distances", {

    test_configurations <- list(
        list(n = 2, k = 1),
        list(n = 4, k = 2 * 3/4),
        list(n = 4, k = 2),
        list(n = 4, k = 3),
        list(n = 20, k = 2.5)
    )

    for (args in test_configurations) {
        graph <- randomGraph(no.of.nodes = args$n, k = args$k)
        results <- floydWarshall(graph)
        reference_value <- distances(graph)
        expect_equal(results$min_dists, reference_value)
    }
})

test_that("FloydWarshall produces valid shortest_path_predecessors", {
    # Example graph from http://nptel.ac.in/courses/105104098/TransportationII/mod13/16slide.htm

    adj <- matrix(c(
        0,8,3,5,0,
        8,0,2,0,5,
        0,1,0,3,4,
        6,0,0,0,7,
        0,5,0,0,0
    ), ncol=5, byrow=T)
    graph <- graph_from_adjacency_matrix(adj, weighted=T)

    A <- 1 #V(graph)[[1]]
    B <- 2 #V(graph)[[2]]
    C <- 3 #V(graph)[[3]]
    D <- 4 #V(graph)[[4]]
    E <- 5 #V(graph)[[5]]
    spp <- matrix(list(
        NULL,C,A,A,C,
        B,NULL,B,C,B,
        c(B,D),C,NULL,C,C,
        D,C,A,NULL,D,
        B,E,B,C,NULL
    ),ncol=5,byrow=T)
    colnames(spp) <- rownames(spp) <- LETTERS[1:5]

    results <- floydWarshall(graph)

    expect_equal(spp, results$shortest_path_predecessors)
})
