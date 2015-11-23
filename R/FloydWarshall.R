#' Calculates the shortest path of all pairs of vertices in a graph
#'
#'
#' @param graph The \code{igraph} object.
#' @return A list of \code{spgraph} objects. Each \code{spgraph} object contains information about
#' a certain step in the optimization process representing by its attributes
#' @import igraph
#'
floydWarshall <- function(graph) {
    graph <- as.spgraph(graph)
    nVertices <- length(V(graph))

    # Create the adjacency matrix with weighted edges (Assumption: weight of edges has to be in the edge
    # attribute 'weight' of the given igraph)
    am <- get.adjacency(graph, attr = "weight", sparse = FALSE)

    # Update modyfied attributes
    graph$min_dists <- replace(graph$min_dists, which(am > 0), am[which(am > 0)])
    graph$shortest_path_predecessors <-
        replace(graph$shortest_path_predecessors, which(am > 0), which(am > 0, arr.ind = TRUE)[, 1])

    # initialization of a list which contains the spg graphs
    steps = list(graph)
    # Add all vertices one by one to the set of intermediate vertices. ---> Before start of a iteration, we
    # have shortest distances between all pairs of vertices such that the shortest distances consider only
    # the vertices in set {0, 1, 2, .. k-1} as intermediate vertices. ----> After the end of a iteration,
    # vertex no. k is added to the set of intermediate vertices and the set becomes {0, 1, 2, .. k}
    for (k in 1:nVertices) {
        # Pick all vertices as source one by one
        for (i in 1:nVertices) {
            # Pick all vertices as destination for the above picked source
            for (j in 1:nVertices) {
                if (i == k || j == k || i == j) {
                    next
                }
                min_dist_current <- graph$min_dists[i, j]
                min_dist_over_k <- graph$min_dists[i, k] + graph$min_dists[k, j]

                if (min_dist_over_k < min_dist_current) {
                    graph$min_dists[i, j] <- min_dist_over_k
                    graph$shortest_path_predecessors[[i, j]] <- k

                } else if (min_dist_over_k == min_dist_current) {
                    if (!(k %in% graph$shortest_path_predecessors[[i, j]])) {
                        graph$shortest_path_predecessors[[i, j]] <-
                            c(graph$shortest_path_predecessors[[i, j]], k)
                    }
                }
            }
        }
        steps = c(steps, list(graph))
    }
    spresults(steps)
}
