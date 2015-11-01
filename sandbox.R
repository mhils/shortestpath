install.packages("rgl")
install.packages("wesanderson")
install.packages("cairoDevice")


library(igraph)
library(igraphdata)
library(wesanderson)
library(cairoDevice)

# List R data sets data(package='igraphdata')

library(formatR)
tidy_dir("./R", arrow=T, width.cutoff=100, recursive=T)
tidy_dir("./tests", arrow=T, width.cutoff=100, recursive=T)


data(karate)

distance_table(karate)
mean_distance(karate)
distances(karate)
plot(karate)

m <- matrix(c(0, 1, 1, 0, 0, 1, 0, 0, 2, 0, 1, 0, 0, 2, 0, 0, 2, 2, 0, 4, 0, 0, 0, 4, 0), ncol = 5)
`?`(graph.adjacency)
g <- graph.adjacency(m, weighted = T, mode = "undirected")
plot(g)

min_dist <- matrix(c(0, Inf, Inf, Inf, Inf), ncol = 1)
graph.attributes(g)$min_dist <- min_dist
shortest_path_predecessor <- matrix(c(NA, NA, NA, NA, NA), ncol = 1)
graph.attributes(g)$shortest_path_predecessor <- shortest_path_predecessor

vertex_states <- as.factor(c("known", "front", "unknown"))

vertex.attributes(g)$label <- c("red", "blue", "A (3)", "blue", "blue")

plot.igraph(g)


d[, 1]
plot.igraph(karate)
d <- as_adjacency_matrix(karate, attr = "weight")
d[, 1]

# tkigraph()
library("rgl")
# rglplot(karate)


# demo(package='igraph')

wes_palette("Royal1")


plot(karate, vertex.color = wes_palette("Royal1"))
Cairo()
plot(karate, vertex.color = wes_palette("Royal1"))



class(karate) <- c("shortestpathgraph", class(karate))
class(karate)

vertex.attributes(karate)$front <- factor(rep("unknown", 34), levels = c("unknown", "front", "known"))
vertex.attributes(karate)$front[c(11, 5, 6, 7, 17)] <- "known"
vertex.attributes(karate)$front[1] <- "front"

d <- data.frame(from = c("D", "D", "D", "H", "S", "D", "H", "S", "L", "A", "S", "W", "S"), to = c("S",
    "H", "L", "L", "L", "W", "W", "W", "A", "B", "B", "F", "F"), weight = c(1, 1, 3, 4, 4, 5, 5, 4, 3,
    4, 9, 5, 8))
example <- graph_from_data_frame(d, directed = F)
class(example) <- c("spgraph", class(example))
vertex.attributes(example)$front <- c("known", "front", "known", rep("unknown", 5))
graph.attributes(example)$min_dist <- matrix(c(0, 1, 1, 3, Inf, 5, 10, 9), ncol = 1)


is.shortestpathgraph <- function(graph) {
    "shortestpathgraph" %in% class(graph)
}

plot.shortestpathgraph <- function(x, vertex.color = "front", edge.label = "weight", vertex.label = "auto",
    ...) {
    if (!is.shortestpathgraph(x)) {
        stop("Not a shortestpathgraph object")
    }
    graph <- x

    if (is.character(vertex.color) && vertex.color == c("front")) {
        vertex.attributes(graph)$color <- factor(vertex.attributes(graph)[[vertex.color]], levels = c("unknown",
            "front", "known", "start"))
        vertex.attributes(graph)$color[1] <- "start"
    }
    if (is.character(edge.label) && edge.label %in% c("weight")) {
        edge.attributes(graph)$label <- edge.attributes(graph)[[edge.label]]
    }
    if (is.character(vertex.label) && vertex.label == c("auto")) {
        names <- vertex.attributes(graph)$name
        dists <- graph.attributes(graph)$min_dist[, 1]
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
set.seed(1)
plot(example)
