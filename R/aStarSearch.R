#' A*-Search Algorithm
#'
#' Use the A*-Search algorithm to solve a shortest path problem.
#'
#' A*-Search is a single-source algorithm which cannot deal with negative edge weights.
#' Compared to Dijkstra's algorithm (\code{\link{dijkstra}}), it uses an euclidean distance heuristic to estimate
#' the minimum distance to the target vertex and thereby rule out solutions.
#' Thus, it is usually vastly faster than Dijkstra, but requires an euclidean problem instance.
#'
#' @param graph The graph object.
#' @param from The source vertex
#' @param to The target vertex
#' @param distance.heuristic The A* distance heuristic.
#' @return An \code{\link{spresults}} object.
#' @examples
#' g <- randomGraph(6, euclidean=TRUE)
#' d <- aStarSearch(g, "A", "F")
#'
#' plot(d)
#'
#' for(step in d){
#'   print(step$min_dists)
#' }
#'
#' @export
#' @seealso
#' \code{\link{setRandomVertexCoordinates}} and \code{\link{setVertexCoordinatesFromLayout}}
#' to set vertex coordinates for a graph object.
#' \code{\link{euclidean.vertex.distance}} for the default distance heuristic.
aStarSearch <- function (graph, from, to, distance.heuristic=euclidean.vertex.distance){

    if(identical(distance.heuristic, euclidean.vertex.distance) && !has.vertex.coordinates(graph)) {
        stop("Cannot compute euclidean distance for vertices without position.")
    }

    graph %<>%
        as.spgraph() %>%
        setRoute(from, to) %>%
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
    spresults(steps)
}

aStarStep <- function(graph, front){

    dist_to_front <- graph$min_dists[1,front]

    for(neighbor in neighbors(graph, front)){
        edge <- E(graph)[front %->% neighbor]
        dist_over_front <- dist_to_front + edge$weight
        current_dist <- graph$min_dists[1,neighbor]

        # path over front is better than the best known path
        if(dist_over_front < current_dist){
            graph$min_dists[1,neighbor] <- dist_over_front
            graph$shortest_path_predecessors[[1, neighbor]] <- c(front)

        # path over front is as good as the best known path
        } else if (dist_over_front == current_dist){
            graph$shortest_path_predecessors[[1, neighbor]] <-
              c(
                graph$shortest_path_predecessors[[1, neighbor]],
                front
              )
        }
    }
    graph
}

aStarNextFront <- function(graph, from, to, distance.heuristic){

    # In the first iteration, we start with the source vertex
    if(length(V(graph)[V(graph)$set == "known"]) == 0){
        from
    } else {
        front_candidates <- V(graph)[V(graph)$set == "unknown"]

        dist <- function(vertex){
            graph$min_dists[1, vertex] + distance.heuristic(graph, to, vertex)
        }

        best_case_distances <- dist(front_candidates)
        lowest_best_case_distances <- which(best_case_distances == min(best_case_distances))
        # We just take the first vertex with lowest best case distance.
        pick <- front_candidates[lowest_best_case_distances[1]]

        # Tricky: Consider a rectangle with perfect euclidean distances.
        #
        # A---B
        # |   |
        # D---C
        #
        # From A to C:
        # - First, we evaluate A, second we evaluate B (D works to, but we pick the first).
        # - Now, we have two front candidates with equal min_dist: C and D.
        #   C is already the destination, we could return this and stop thereby;
        #   however, we want _all_ shortest paths. Thus, we must not return C
        #   if there is a second alternative with equal best case distance.
        if(pick == to && length(lowest_best_case_distances) > 1){
            pick <- front_candidates[lowest_best_case_distances[2]]
        }
        pick
    }
}
