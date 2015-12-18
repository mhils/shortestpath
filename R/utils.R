#' Euclidean distance computation for vertices with \code{x} and \code{y} attributes.
#' @param graph The igraph object
#' @param v1 The first vertice vector
#' @param v2 The second vertice vector.
#' @export
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
        } else if(length(v) == 0){
            stop(paste0("Identifier '",paste(identifier,collapse=" "), "' selected no vertices."))
        }
        v
    }
}


is_edge_intersection <- function(g, edges_or_vertices) {
    if(!has.vertex.coordinates(g)){
        stop("Cannot determine edge intersections without vertex coordinates")
    }
    if(length(edges_or_vertices) == 2) {
        point_names <- ends(g, edges_or_vertices)
    } else if(length(edges_or_vertices) == 4) {
        point_names <- edges_or_vertices
    } else {
        stop(paste("Invalid edges_or_vertices:", edges_or_vertices))
    }
    points <- apply(point_names, c(1,2), function(x_) {
        v <- V(g)[x_]
        c(v$x, v$y)
    })
    if(all(points[,1,] == points[,2,]) ||
       all(points[,1,] == points[,2,2:1])){
        return(TRUE)
    }
    # Convert lines to parametric form: x = u + s*v
    u1 <- points[,1,1]
    x1 <- points[,1,2]
    v1 <- x1 - u1
    u2 <- points[,2,1]
    x2 <- points[,2,2]
    v2 <- x2 - u2
    # Find intersection
    # u1 + s1*v1 = u2 + s2*v2 <=>
    # s1*v1 - s2*v2 = u2 - u1
    # Solve linear equation:
    s <- try(solve(cbind(v1,-v2), u2-u1), silent=TRUE)
    # If 0 < s < 1, the intersection is still on the line.
    # If 0 == s || 1 == s, the edges share one vertex as an end.
    # We do not want to treat this as an intersection.
    !inherits(s, "try-error") && all(0.01 < s & s < 0.99)
}
