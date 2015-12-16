#' Create a random regular graph
#'
#' Generate a "nice" random graph with a fixed average vertex degree
#' and at most one cluster.
#'
#' @param k The average degree of each vertex in the graph
#' @inheritParams igraph::sample_k_regular
#' @export
sample_average_k_connected_graph <- function(no.of.nodes, k){
    # To get "nice" random graphs, we create a sample_k_regular graph
    # with a degree of k*2 and then randomly remove edges. This creates graphs
    # which aren't too degenerated.

    # Check if k is valid (We must do this manually because sample_k can be != k)
    if(k > no.of.nodes - 1){
        stop("The graph degree must not exceed #N-1 for a regular graph")
    }
    if(k < 2 * (no.of.nodes - 1) / no.of.nodes){
        stop("The graph degree k must be >= 2 * (no.of.nodes-1)/no.of.nodes")
    }

    sample_k <- k * 2

    repeat {

        # We have to cap sample_k at no.of.nodes as there's a maximum of one
        # edge between every node.
        sample_k <- min(sample_k, no.of.nodes - 1)

        g <- sample_k_regular(no.of.nodes, sample_k)
        if(no.clusters(g) == 1){
            break
        }
        # Slightly increasae sample_k to increase odds of a single connected component.
        sample_k <- sample_k + 1/no.of.nodes
    }

    maxEdges <- round((no.of.nodes*k) / 2)
    while(length(E(g)) > maxEdges){
        g2 <- delete.edges(g, sample(E(g),1))
        if(no.clusters(g2) == 1){
            g <- g2
        }
    }
    g
}

removeOverlappingEdges <- function(graph) {
    repeat {
        edges <- sample(E(graph))
        edge_pairs <- combn(edges, 2)
        #edge_ends <- apply(edge_pairs, c(1,2), function(x) ends(graph, x))
        for(i in seq_len(ncol(edge_pairs))) {
            e1 <- edge_pairs[1,i]
            e2 <- edge_pairs[2,i]
            if(is_edge_intersection(graph, e1, e2)){
                edge_to_remove <- sample(c(e1,e2), 1)
                g2 <- delete_edges(graph, edge_to_remove)
                if(no.clusters(g2) > 1){
                    next
                }

                    # add edge (that does not overlap)
                    repeat {
                        new_vertex_ends <- sample(V(graph), 2)
                        g2_ends <- ends(g2, E(g2))
                        # g2 <- add_edges(g2, vertex_ends)
                        for(i in seq_len(nrow(g2_ends))) {
                            if(is_edge_intersection(
                                g2,
                                cbind(new_vertex_ends, g2_ends[,i])
                            )) {
                                next
                            }
                        }
                        graph <- add_edges(g2, new_vertex_ends)
                        break
                    }

                }
            }

        }
    }
}

#' Random Graph Generation
#'
#' This method generates a random graph suitable for exercises.
#'
#' @param euclidean Set to \code{TRUE} if (rounded) euclidean distances should be used.
#' @inheritParams sample_average_k_connected_graph
#' @export
randomGraph <- function(no.of.nodes=12, k=2.5, euclidean=FALSE) {

    g <- sample_average_k_connected_graph(no.of.nodes, k) %>%
        as.spgraph()

    if(euclidean == TRUE) {
        g %>% setEuclideanEdgeWeights()
    } else {
        g %>% setRandomEdgeWeights()
    }
}
