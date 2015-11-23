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
    } else if(is.logical(identifier) && identifier == TRUE){
        V(graph)[1]
    } else {
        v <- V(graph)[identifier]
        if(length(v) > 1){
            stop(paste0("Identifier '",paste(identifier,collapse=" "), "' selected more than one vertex: ", paste(v,collapse=" ")))
        }
        v
    }
}

# get.shortest.paths <- function(graph, from=NULL, to=NULL) {
#     print(paste(from,to))
#     if(!is.spgraph(graph) && !is.spresults(graph)){
#         stop("graph is neither an spgraph nor an spresults object.")
#     }
#     if(is.null(from)){
#         from <- graph$from
#     }
#     if(is.null(to)){
#         to <- graph$to
#     }
#     if(from == to){
#         return(c(to))
#     }
#     predecessors <- graph$shortest_path_predecessors[[to,from]]
#     lapply(predecessors, function(p) {
#         print(paste("Run for",p))
#         c(get.shortest.paths(graph, from, p), to)
#     })
# }
#
#set.seed(20)
#graph <- randomGraph() %>%
#    setVertexCoordinatesFromLayout(layout.fruchterman.reingold, list(niter=10000)) %>%
#    setEuclideanEdgeWeights()
#plot(graph)
#
#
#?aStarSearch
#result <- aStarSearch(graph,"C","J")
#get.shortest.paths(result)
