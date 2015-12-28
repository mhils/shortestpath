#' Floyd-Warshall Algorithm
#'
#' Use the Floyd-Warshall algorithm to calculate the shortest path between
#' all pairs of vertices in a directed, weighted graph.
#'
#' The Floyd-Warshall algorithm is a multi-source algorithm which can (in
#' contrast to Dijkstra and A*-Search) deal with negative edge
#' weights. Note that in order to find the right shortest path, it is required that
#' no negative-weight cycle exist in the graph. The algorithm automatically
#' detects negative-weight cycles and shows a corresponding error message.
#'
#' The algorithm updates the minimal distance of each vertex pair #V number of times.
#' Thus, the running time complexity of the algorithm is #V^3.
#'
#' Caveat: The computation of the distances is based on an adjacency matrix.
#' A limitation in igraph mandates that an edge weight of zero indicates
#' that there exist no edge between two vertices.
#'
#' @param graph The \code{igraph} object.
#' @examples
#' g <- randomGraph(6,euclidean=FALSE)
#'
#' fw <- floydWarshall(g)
#'
#' plot(fw)
#'
#' for(step in fw){
#'    print(step$min_dists)
#' }
#'
#' @return An \code{\link{spresults}} object.
#' @export
#'
floydWarshall <- function(graph) {
    graph <- as.spgraph(graph)

    # Take all direct connections from the adjacency matrix and update the graph's min_dists and
    # shortest path predecessors from it.
    weight.attr <- "weight"
    if(ecount(graph) == 0){
        weight.attr <- NULL # Otherwise this will throw an error.
    }
    am <- get.adjacency(graph, attr = weight.attr, sparse = FALSE)
    direct_connections <- which(am != 0, arr.ind = TRUE)
    direct_min_dists <- replace(graph$min_dists, direct_connections, am[direct_connections])
    direct_spp <- replace(graph$shortest_path_predecessors, direct_connections, direct_connections[, 1])

    graph %<>%
        set_graph_attr("min_dists", direct_min_dists) %>%
        set_graph_attr("shortest_path_predecessors", direct_spp)

    steps = list(graph)
    # Add all vertices one by one to the set of intermediate vertices. ---> Before start of a iteration, we
    # have shortest distances between all pairs of vertices such that the shortest distances consider only
    # the vertices in set {0, 1, 2, .. k-1} as intermediate vertices. ----> After the end of a iteration,
    # vertex no. k is added to the set of intermediate vertices and the set becomes {0, 1, 2, .. k}
    nVertices <- seq_len(vcount(graph))
    for (k in nVertices) {

        min_dists <- graph$min_dists
        shortest_path_predecessors <- graph$shortest_path_predecessors

        # Pick all vertices as source one by one
        for (i in nVertices) {
            # Pick all vertices as destination for the above picked source
            for (j in nVertices) {
                if (i == k || j == k) {
                    next
                }
                min_dist_current <- min_dists[i, j]
                min_dist_over_k <- min_dists[i, k] + min_dists[k, j]

                # Check if this is the new best solution.
                if (min_dist_over_k < min_dist_current) {
                    min_dists[i, j] <- min_dist_over_k
                    shortest_path_predecessors[[i, j]] <- k

                # Check if we are as good as the current best solution
                } else if (min_dist_over_k == min_dist_current && !is.infinite(min_dist_current)) {
                    # In this case, we still add it as a spp (if not already present)
                    if (!(k %in% shortest_path_predecessors[[i, j]])) {
                        shortest_path_predecessors[[i, j]] <-
                            c(shortest_path_predecessors[[i, j]], k)
                    }

                }

            }
        }

        graph %<>%
            set_graph_attr("min_dists", min_dists) %>%
            set_graph_attr("shortest_path_predecessors", shortest_path_predecessors)
        steps = c(steps, list(graph))
    }
    if(any(diag(min_dists) < 0)){
        stop("graph has a negative cycle ")
    }
    spresults(steps)
}
