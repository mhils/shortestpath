aStarSearch <- function (graph, from, to, distance.heuristic=euclidean.vertex.distance){

    graph %<>%
        makeShortestPathGraph(from, to) %>%
        setVertexSets("unknown")

    steps = list()

    while(TRUE) {
        front <- aStarNextFront(graph, graph$from, graph$to, distance.heuristic)
        graph %<>% set_vertex_attr("set", front, "front")
        if(graph$to == front) {
            steps <- c(steps, list(graph))
            break
        }
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

aStarNextFront <- function(graph, from, to, distance.heuristic){

    # In the first iteration, we start with the source vertex
    if(length(V(graph)[set == "known"]) == 0){
        from
    } else {
        front_candidates <- V(graph)[set == "unknown"]

        dist <- function(vertex){
            graph$min_dists[vertex] + distance.heuristic(graph, to, vertex)
        }

        print(rbind(front_candidates, dist(front_candidates)))

        shortest_distance <- which.min(dist(front_candidates))

        front_candidates[shortest_distance]
    }
}
