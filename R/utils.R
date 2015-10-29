#' Plotting of shortest path graphs
#'
#' @param graph The graph object.
#' @return \code{TRUE} if its argument is a shortest path graph, \code{FALSE} otherwise.
#' @export
is.spgraph <- function(graph) {
    "spgraph" %in% class(graph)
}

euc.dist <- function(graph, v1, v2) {
    if(!has.vertex.coordinates(graph)) {
        stop("Cannot compute euclidean distance for vertices without position.")
    }
    x1 <- vertex_attr(graph, "x", v1)
    y1 <- vertex_attr(graph, "y", v1)
    x2 <- vertex_attr(graph, "x", v2)
    y2 <- vertex_attr(graph, "y", v2)
    sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
}

has.vertex.coordinates <- function(graph){
    (
        !is.null(V(graph)$x)
        &&
        !is.null(V(graph)$y)
    )
}

# convenience function that returns the vertex object associated with the
# given input, which is either the vertex object itself or a vertex identifier
get.vertex <- function(graph, x){
    if(inherits(x, "igraph.vs")){
        x
    } else if(x == TRUE){
        V(graph)[1]
    } else {
        v <- V(graph)[x]
        if(length(v) > 1){
            stop(paste("More than one vertex:", x))
        }
        v
    }
}
