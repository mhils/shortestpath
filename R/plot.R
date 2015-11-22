#' Plotting of shortest path graphs
#'
#' \code{plot.spgraph} is a wrapper around \code{\link{plot.igraph}} providing
#' convenience features for shortest path graph plotting.
#'
#' @param x The graph to plot.
#' @param edge.label The edge label. Passing 'weight' will print the edge's weight.
#' @param vertex.label The vertex label. Passing 'auto' will print the vertex's name
#' and its currently known minimal distance.
#' @param vertex.color Color palette for the vertices
#' @param vertex.color.by Characteristic which should be used to color vertices
#' @param vertex.frame.color Color palette for the vertices' frame.
#' @param vertex.frame.color.by Characteristic which should be used to color vertex frames
#' @param ... All other parameters are passed to \code{\link{plot.igraph}} as is.
#'
#' @examples
#' # TODO
#'
#' @export
plot.spgraph <- function(x,
                         vertex.color = wes_palette("Royal1")[c(3,4,1)],
                         vertex.color.by = c("set","manual"),
                         vertex.frame.color = wes_palette("Rushmore")[c(4,3,5)],
                         vertex.frame.color.by = c("type","manual"),
                         edge.label = "weight",
                         vertex.label = "auto",
                         ...) {
    if (!is.spgraph(x)) {
        stop("Not a spgraph object")
    }
    graph <- x

    if(match.arg(vertex.color.by) == "set"){
        V(graph)$color <- vertex.color[1]
        V(graph)[V(graph)$set=="front"]$color <- vertex.color[2]
        V(graph)[V(graph)$set=="unknown"]$color <- vertex.color[3]
    } else {
        V(graph)$color <- vertex.color # nocov
    }
    if(match.arg(vertex.frame.color.by) == "type"){
        # FIXME: This is not color.
        V(graph)$size <- 15
        V(graph)[c(graph$from, graph$to)]$size <- 20

        V(graph)$frame.color <- vertex.frame.color[1]
        V(graph)[graph$from]$frame.color <- vertex.frame.color[2]
        V(graph)[graph$to]$frame.color <- vertex.frame.color[3]
    } else {
        V(graph)$frame.color <- vertex.frame.color # nocov
    }
    if (is.character(edge.label) && edge.label %in% c("weight")) {
        edge.attributes(graph)$label <- edge.attributes(graph)[[edge.label]]
    }
    if (is.character(vertex.label) && vertex.label == c("auto")) {
        names <- vertex.attributes(graph)$name
        dists <- graph.attributes(graph)$min_dists[, 1]
        dists <- vapply(dists, function(x) {
            # wtf: '\u221e' (the infinity symbol) will be printed as '8'
            ifelse(x == Inf, "?", as.character(x))
        }, "")
        labels <- sapply(1:length(dists), function(i) {
            paste(names[i], " (", dists[i], ")", sep = "")
        })
        vertex.attributes(graph)$label <- labels
    }

    plot.igraph(graph,
                palette = wes_palette("Royal1"),
                edge.label.color = "black",
                vertex.label.dist = 1,
        ...)
}
