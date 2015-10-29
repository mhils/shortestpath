#' Plotting of shortest path graphs
#'
#' @param graph The graph object.
#' @return \code{TRUE} if its argument is a shortest path graph, \code{FALSE} otherwise.
#' @export
is.spgraph <- function(graph) {
    "spgraph" %in% class(graph)
}

#' Euclidean distance computation for vertices with \code{x} and \code{y} attributes.
#' @param graph The igraph object
#' @param v1 The first vertice vector
#' @param v2 The second vertice vector.
euclidean.vertex.distance <- function(graph, v1, v2) {
    if(!has.vertex.coordinates(graph)) {
        stop("Cannot compute euclidean distance for vertices without position.")
    }
    x1 <- vertex_attr(graph, "x", v1)
    y1 <- vertex_attr(graph, "y", v1)
    x2 <- vertex_attr(graph, "x", v2)
    y2 <- vertex_attr(graph, "y", v2)
    sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
}

#' Returns \code{TRUE} if the graph's vertices have non-null coordinates.
#' @param graph The graph to check
has.vertex.coordinates <- function(graph){
    (
        !is.null(V(graph)$x)
        &&
            !is.null(V(graph)$y)
    )
}

#' Convenience function that returns the vertex object associated with the
#' given identifier, which is either the vertex object itself or a vertex id.
#' @param graph The igraph object
#' @param identifier the identifier
get.vertex <- function(graph, identifier){
    if(inherits(identifier, "igraph.vs")){
        identifier
    } else if(identifier == TRUE){
        V(graph)[1]
    } else {
        v <- V(graph)[identifier]
        if(length(v) > 1){
            stop(paste("More than one vertex:", identifier))
        }
        v
    }
}
