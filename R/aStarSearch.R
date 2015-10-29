aStarSearch <- function (graph, from, to, min.dist.fun=euc.dist){

    graph %<>%
        makeShortestPathGraph(from, to) %>%
        setUniformVertexSets(val="unknown")

    from <- get.vertex(graph, from)
    to <- get.vertex(graph, to)

    steps = list(graph)

    while(TRUE) {
        front <- aStarNextFront(graph, from, min.dist.fun)
        if(to == front) {
            break
        }
        graph %<>% set_vertex_attr("set", front, "front")
        graph %<>% aStarStep(front)
        steps <- c(steps, list(graph))
        graph %<>% set_vertex_attr("set", front, "known")
    }
    steps
}

aStarStep <- function(graph, front){

    dist_to_front <- graph$min_dists[front]

    for(neighbor in neighbors(graph, front)){
        edge <- E(graph)[front %--% neighbor]
        dist_over_front <- dist_to_front + edge$weight

        # path over front is better than the best known path
        if(dist_over_front < graph$min_dists[neighbor]){
            graph$min_dists[neighbor] <- dist_over_front
            graph$shortest_path_predecessors[[neighbor]] <- c(front)

        # path over front is as good as the best known path
        } else if (dist_over_front == graph$min_dists[neighbor]){
            graph$shortest_path_predecessors[[neighbor]] <- c(graph$shortest_path_predecessors, front)
        }
    }
    graph
}

aStarNextFront <- function(graph, from, min.dist.fun){

    # In the first iteration, we start with the source vertex
    if(length(V(graph)[set == "known"]) == 0){
        from
    } else {
        front_candidates <- V(graph)[set == "unknown"]

        dist <- function(vertex){
            graph$min_dists[vertex] + min.dist.fun(graph, from, vertex)
        }

        shortest_distance <- which.min(dist(front_candidates))

        front_candidates[shortest_distance]
    }
}


# graph <- sample_average_k_connected_graph(8, 2.5) %>%
#     setAlphabeticalVertexNames() %>%
#     setRandomVertexCoordinates() %>%
#     setEuclideanEdgeWeights()
# graph <- thegraph
# plot(graph)
# r <- aStarSearch(graph,"A","G")
# par(mfrow=c(3,2))
# for(p in r){
#     plot(p)
# }
#
# list(
#     steps=list(),
#     from=,
#     to=,
#     algorithm=,
# )
