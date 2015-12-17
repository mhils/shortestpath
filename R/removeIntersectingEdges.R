# adds an edge to a graph that does not intersect with any
# other edges.
addNonIntersectingEdge <- function(graph) {

    nodes <- sample(V(graph))
    node_pairs <- combn(nodes, 2)
    graph_ends <- ends(graph, E(graph), names=FALSE)

    for(i in seq_len(ncol(node_pairs))) {
        new_vertex_ends <- node_pairs[,i]
        # Speed up the common case
        if(are_adjacent(graph, new_vertex_ends[1], new_vertex_ends[2])){
            next
        }
        no_overlaps <- TRUE
        for(i in seq_len(nrow(graph_ends))) {
            if(is_edge_intersection(
                graph,
                rbind(new_vertex_ends, graph_ends[i,])
            )) {
                no_overlaps <- FALSE
                break
            }
        }
        if(no_overlaps == TRUE){
            return(add_edges(graph, new_vertex_ends))
        }
    }
    warning("Unable to add nonoverlapping edge")
    return(graph)
}

#' Remove edge intersections
#'
#' This function tries to eliminate edge intersections in a graph by
#' removing intersected edges and re-adding new edges (that do not
#' intersect any other edges). Newly added edges will not have any edge attributes.
#'
#' This is a fuzzy algorithm, there is no guarantee that all intersections are removed.
#' If \code{relayout} is \code{FALSE}, however, it is guaranteed that a warning will be emitted in this case.
#'
#' @param graph The graph object.
#' @param relayout If \code{TRUE}, the Fruchterman-Reingold layout algorithm
#' is re-applied after removing edges. This may introduce new intersections, but
#' generally improves the quality of the graph.
#'
#' @export
removeIntersectingEdges <- function(graph, relayout=FALSE) {
    edges <- sample(E(graph))
    edge_pairs <- combn(edges, 2)

    final_graph <- graph

    for(i in seq_len(ncol(edge_pairs))) {
        edge_pair <- edge_pairs[,i]
        if(is_edge_intersection(graph, edge_pair)){
            g2 <- delete_edges(final_graph, edge_pair[1])
            if(no.clusters(g2) > 1){
                g2 <- delete_edges(final_graph, edge_pair[2])
                if(no.clusters(g2) > 1){
                    warning("Cannot remove edge without creating a second cluster")
                    next
                }
            }
            final_graph <- addNonIntersectingEdge(g2)
        }
    }
    final_graph %<>% as.spgraph()
    if(relayout == TRUE){
        final_graph %<>% setVertexCoordinatesFromLayout(
            .,
            layout_with_fr,
            layout_args=list(coords=layout_nicely(.), niter=2000)
        )
    }
    final_graph
}
