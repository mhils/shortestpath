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
    x <- vertex_attr(graph, "x", c(v1,v2))
    y <- vertex_attr(graph, "y", c(v1,v2))
    d <- sqrt((x[1] - x[2]) ^ 2 + (y[1] - y[2]) ^ 2)
    d
}

has.vertex.coordinates <- function(x){
    !(
        is.null(V(graph)$x)
        ||
            is.null(V(graph)$y)
    )
}

get.vertex <- function(graph, x){
    if(inherits(x, "igraph.vs")){
        x
    } else {
        V(graph)[x]
    }
}
