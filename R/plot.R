#' Plotting of shortest path graphs
#'
#' \code{plot.spgraph} is a wrapper around \code{\link{plot.igraph}} providing
#' convenience features for shortest path graph plotting.
#'
#' Vertex/edge visuals depending on the underlying graph characteristics:
#'
#' \describe{
#'  \item{\code{vertex.color.by}}{ denotes which attribute should be used to set the vertex color}
#'  \item{\code{vertex.color}}{ specifies the color palette, or, if \code{.by} is \code{"manual"}, a color for each vertex.}
#' }
#' Available vertex characteristics:
#' \itemize{
#'  \item{\code{set}}{ The set to which the vertex belongs: processed/front/unknown}
#'  \item{\code{type}}{ start node/normal/destination node}
#'  \item{\code{manual}}{ A color for each vertex is provided manually}
#' }
#'
#' Available edge characteristics:
#' \itemize{
#'  \item{\code{shortestpath}}{ edge is not on the shortest path / edge is on the shortest path}
#'  \item{\code{manual}}{ A color for each edge is provided manualy}
#' }
#'
#' @param x The graph to plot.
#' @param vertex.color.by Characteristic which should be used to color vertices
#' @param vertex.color Color palette for the vertices
#' @param vertex.frame.color.by Characteristic which should be used to color vertex frames
#' @param vertex.frame.color Color palette for the vertices' frame.
#' @param vertex.size.by Characteristic which should be used to determine vertex sizes
#' @param vertex.size The size options
#' @param edge.color.by Characteristic which should be used to color edges
#' @param edge.color Color palette for the edges
#' @param vertex.label The vertex label.
#' @param edge.label The edge label.
#' @param edge.label.family See \code{\link{igraph.plotting}}
#' @param edge.label.color See \code{\link{igraph.plotting}}
#' @param edge.label.cex See \code{\link{igraph.plotting}}
#' @param vertex.label.color See \code{\link{igraph.plotting}},
#' @param vertex.label.dist See \code{\link{igraph.plotting}}
#' @param vertex.label.cex See \code{\link{igraph.plotting}}
#' @param vertex.label.family See \code{\link{igraph.plotting}}
#' @param vertex.label.degree See \code{\link{igraph.plotting}}
#' @param default.margins If \code{TRUE}, plot margins will be zeroed.
#' @param ... All other parameters are passed to \code{\link{plot.igraph}} as is.
#'
#' @examples
#' g <- randomGraph()
#' d <- dijkstra(g, "A", "B")
#' plot(d)
#'
#' @export
plot.spgraph <- function(x,
                         # Colors
                         vertex.color.by = c("set", "type", "manual"),
                         vertex.color = wes_palette("Royal1")[c(3,4,1)],
                         vertex.frame.color.by = c("type", "set", "manual"),
                         vertex.frame.color = wes_palette("Rushmore")[3:5],
                         vertex.size.by = c("type", "set", "manual"),
                         vertex.size = c(15, 25, 25),
                         edge.color.by = c("shortestpath", "manual"),
                         edge.color = c("darkgrey", wes_palette("Darjeeling")[4]),
                         vertex.label = nice_vertex_labels(x),
                         edge.label = E(graph)$weight,
                         # Reasonable defaults for igraph builtins.
                         edge.label.family="sans",
                         edge.label.color = "#444444",
                         edge.label.cex = 0.75,
                         vertex.label.color = nice_vertex_label_colors(x),
                         vertex.label.dist = 1.25,
                         vertex.label.cex = 0.8,
                         vertex.label.family="sans",
                         vertex.label.degree = -pi/7,
                         default.margins = TRUE,
                         ...) {
    graph <- x

    # You almost always want 0 margins with igraph, so we set this by default.
    if(default.margins == TRUE){
        par(mar=c(1,1,1,1))
    }

    # We need to do this, otherwhise R's lazy-eval screws it up.
    vertex.color.by <- match.arg(vertex.color.by)
    vertex.frame.color.by <- match.arg(vertex.frame.color.by)
    vertex.size.by <- match.arg(vertex.size.by)
    edge.color.by <- match.arg(edge.color.by)

    # Start coloring
    graph %<>%
        set_vertex_attr_by("color", by=vertex.color.by, vertex.color) %>%
        set_vertex_attr_by("frame.color", vertex.frame.color.by, vertex.frame.color) %>%
        set_vertex_attr_by("size", vertex.size.by, vertex.size) %>%
        set_edge_attr_by("color", edge.color.by, edge.color) %>%
        set_vertex_attr("label", value=vertex.label) %>%
        set_edge_attr("label", value=edge.label)

    plot.igraph(graph,
                edge.label.family=edge.label.family,
                edge.label.color = edge.label.color,
                edge.label.cex = edge.label.cex,
                vertex.label.color = vertex.label.color,
                vertex.label.dist = vertex.label.dist,
                vertex.label.cex = vertex.label.cex,
                vertex.label.family= vertex.label.family,
                vertex.label.degree = vertex.label.degree,
                ...
    )
}

set_vertex_attr_by <- function(graph, name, by=c("type", "set", "manual"), value) {
    if(match.arg(by) == "type") {
        graph %>%
            set_vertex_attr(name, value=value[2]) %>%
            set_vertex_attr(name, graph$from, value[1]) %>%
            set_vertex_attr(name, graph$to, value[3])
    } else if(match.arg(by) == "set") {
        graph %>%
            set_vertex_attr(name, value=value[1]) %>%
            set_vertex_attr(name, V(graph)[V(graph)$set=="front"], value[2]) %>%
            set_vertex_attr(name, V(graph)[V(graph)$set=="unknown"], value[3])
    } else {
        graph %>%
            set_vertex_attr(name, value=value)
    }
}

set_edge_attr_by <- function(graph, attr, by=c("shortestpath", "manual"), value) {
    if(match.arg(by) == "shortestpath") {
        sp_edges = c()

        if(graph$from != FALSE){
            for(shortest_path in getShortestPaths(graph)){
                sp_edges <- c(sp_edges, shortest_path$edges)
            }
        }

        graph %>%
            set_edge_attr(attr, value=value[1]) %>%
            set_edge_attr(attr, E(graph)[sp_edges], value[2])

    } else {
        graph %>%
            set_edge_attr(attr, value=value)
    }
}

#' Produce a vector of suitable vector colors depending on the vertex' state.
#' @param graph The spgraph object.
#' @param previous The spgraph object of the last iteration. If provided, changed min_dists
#' will be highlighted.
#' @param colors A \code{c(normal, infinite distance, highlight)} color tuple
#' @export
nice_vertex_label_colors <- function(graph, previous=NULL, colors=c("black", "darkgray", wes_palette("Royal1")[2])) {
    if(graph$from == FALSE){
        return(colors[1])
    }

    label_colors <- rep(colors[1], vcount(graph))

    dists <- graph$min_dists[graph$from$name,]
    if(!all(dists==Inf)) {
        label_colors[dists==Inf] <- colors[2]
    }

    if(!is.null(previous)){
        old_dists <- previous$min_dists[graph$from$name,]
        label_colors[dists!=old_dists] <- colors[3]
    }

    label_colors
}

#' Produce "name (current min dist)" labels for all vertices.
#' @param graph The spgraph object.
#' @export
nice_vertex_labels <- function(graph) {
    if(graph$from == FALSE){
        return(V(graph)$name)
    }
    names <- V(graph)$name
    dists <- graph$min_dists[graph$from$name,]
    dists <- vapply(dists, function(x) {
        if(x == Inf){
            "?" # Encoding("\u221e")
        } else {
            as.character(x)
        }
    }, "")
    labels <- sapply(1:length(dists), function(i) {
        paste(names[i], " (", dists[i], ")", sep = "")
    })
    labels
}

#' @export
plot.spresults <- function(x, ...){
    prev <- x[[1]] %>% setInfiniteMinDists()
    for(graph in x){
        plot.spgraph(
            graph,
            vertex.label.color=nice_vertex_label_colors(graph, prev),
            ...
        )
        prev <- graph
    }
}
#plot(a)
