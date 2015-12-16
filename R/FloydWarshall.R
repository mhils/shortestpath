#' Calculates the shortest path of all pairs of vertices in a graph
#'
#'
#' @param graph The \code{igraph} object.
#' @param weight.attr Either \code{NULL} or a character string giving an edge attribute name for the edge cost.
#' @return A list of \code{spgraph} objects. Each \code{spgraph} object contains information about
#' a certain step in the optimization process representing by its attributes
#' @import igraph
#'
floydWarshall <- function(graph, weight.attr="weight") {
    graph <- as.spgraph(graph)

    # Take all direct connections from the adjacency matrix and update the graph's min_dists and
    # shortest path predecessors from it.
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
