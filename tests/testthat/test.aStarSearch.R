context("aStarSearch")

test_that("aStarSearch runs without errors", {

    graph <- randomGraph(n=20,euclidean=TRUE)
    r <- aStarSearch(graph,"A","K")
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

# An euclidean square graph.
#
# D---C
# |   |
# A---B
#
#
make_square <- function(){
    square <- matrix(c(
        0,1,0,1,
        1,0,1,0,
        0,1,0,1,
        1,0,1,0
    ), nrow=4) %>%
        graph_from_adjacency_matrix(mode="undirected", weighted=TRUE)
    V(square)$name <- LETTERS[1:4]
    V(square)$x <- c(0, 1, 1, 0)
    V(square)$y <- c(0, 0, 1, 1)
    square
}

test_that("aStarSearch produces valid min_dists", {
    r <- aStarSearch(make_square(),"A","B")
    expect_equal( as.vector(r[[length(r)]]$min_dists), c(0,1,Inf,1))

    r <- aStarSearch(make_square(),"A","C")
    expect_equal( as.vector(r[[length(r)]]$min_dists), c(0,1,2,1))
})

test_that("aStarSearch produces valid shortest_path_predecessors", {
    square <- make_square()
    r <- aStarSearch(square,"A","B")
    spp <- r[[length(r)]]$shortest_path_predecessors
    ref <- matrix(list(), ncol=1, nrow=4)
    ref[[2]] <- V(square)[[1]]
    ref[[4]] <- V(square)[[1]]
    expect_equivalent(spp, ref)

    r <- aStarSearch(square,"A","C")
    spp <- r[[length(r)]]$shortest_path_predecessors
    ref[[3]] <- c(V(square)[[2]],V(square)[[4]])
    expect_equivalent(spp, ref)
})
