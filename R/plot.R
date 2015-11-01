#' Plotting of shortest path graphs
#'
#' \code{plot.spgraph} is a wrapper around \code{\link{plot.igraph}} providing
#' convenience features for shortest path graph plotting.
#'
#' @param x The graph to plot.
#' @param vertex.color The vertex coloring. Passing 'set' will color,
#' the vertices according to the set they are in, other values are passed to
#' igraph as-is.
#' @param edge.label The edge label. Passing 'weight' will print the edge's weight.
#' @param vertex.label The vertex label. Passing 'auto' will print the vertex's name
#' and its currently known minimal distance.
#' @param ... All other parameters are passed to \code{\link{plot.igraph}} as is.
#'
#' @examples
#' # TODO
#'
#' @export
plot.spgraph <- function(x, vertex.color = "set", edge.label = "weight", vertex.label = "auto", ...) {
    if (!is.spgraph(x)) {
        stop("Not a spgraph object")
    }
    graph <- x
    
    if (is.character(vertex.color) && vertex.color %in% c("set")) {
        vertex.attributes(graph)$color <- factor(vertex.attributes(graph)[[vertex.color]], levels = c("unknown", 
            "front", "known", "start"))
        vertex.attributes(graph)$color[1] <- "start"
    }
    if (is.character(edge.label) && edge.label %in% c("weight")) {
        edge.attributes(graph)$label <- edge.attributes(graph)[[edge.label]]
    }
    if (is.character(vertex.label) && vertex.label == c("auto")) {
        names <- vertex.attributes(graph)$name
        dists <- graph.attributes(graph)$min_dists[, 1]
        dists <- vapply(dists, function(x) {
            # ultra-wtf: '∞' will be printed as '8'
            ifelse(x == Inf, "?", as.character(x))
        }, "")
        labels <- sapply(1:length(dists), function(i) {
            paste(names[i], " (", dists[i], ")", sep = "")
        })
        vertex.attributes(graph)$label <- labels
    }
    
    plot.igraph(graph, palette = wes_palette("Royal1"), edge.label.color = "black", vertex.label.dist = 1, 
        ...)
} 
