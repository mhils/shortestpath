#' Graph Modification
#'
#' shortestpath provides various graph modification functions that are used internally.
#'
#' @examples
#'
#' library(igraph)
#'
#' g <- random.graph.game(10, 0.5) %>%
#'   setAlphabeticalVertexNames %>%
#'   setRandomEdgeWeights()
#'
#' plot(g)
#' E(g)$weight
#'
#'
#' @param graph The original graph object. Will not be modified.
#' @param overwrite If \code{FALSE}, graphs with an existing attribute will not
#' be updated.
#' @return The updated graph object.
#' @name graphModification
#' @rdname graphModification
NULL

# This function does the actual object modification by calling igraph's [set_](graph|vertex|edge)_attr.
setAttr <- function(graph, type = c("graph", "vertex", "edge"), name, fun, overwrite) {

    type <- match.arg(type)
    current <- do.call(paste0(type, "_attr"), list(graph, name))
    if (overwrite || is.null(current)) {
        val <- fun(graph)
        graph <- do.call(paste0("set_", type, "_attr"), list(graph, name, value = val))
    }
    graph
}

#' @describeIn graphModification Set the minimum distance matrix to infinity.
#' @export
setInfiniteMinDists <- function(graph, overwrite = TRUE) {
    setAttr(graph, "graph", "min_dists", function(graph) {
        n <- vcount(graph)
        mat <- matrix(Inf, ncol = n, nrow = n)
        diag(mat) <- c(0)
        colnames(mat) <- V(graph)$name
        rownames(mat) <- V(graph)$name
        mat
    }, overwrite)
}

#' @describeIn graphModification Set random vertex positions for euclidean algorithms.
#' @export
setRandomVertexCoordinates <- function(graph, overwrite = TRUE) {
    graph %>%
        setAttr("vertex", "x", function(graph) {
            runif(vcount(graph), 0, 10)
        }, overwrite) %>%
        setAttr("vertex", "y", function(graph) {
            runif(vcount(graph), 0, 10)
        }, overwrite)
}

#' @describeIn graphModification Run the given layout algorithm,
#' and set the positioning suggested by the algorithm as vertex positions for euclidean algorithms.
#' @param layout An igraph layout function. See \code{igraph::\link[igraph]{layout}}.
#' @export
setVertexCoordinatesFromLayout <- function(graph, layout=nicely(), overwrite = TRUE) {
    p <- layout_(graph, layout)
    graph %>%
        setAttr("vertex", "x", function(graph) {
            p[,1]
        }, overwrite) %>%
        setAttr("vertex", "y", function(graph) {
            p[,2]
        }, overwrite)
}

#' @describeIn graphModification Set rounded euclidean edge weights.
#' @export
setEuclideanEdgeWeights <- function(graph, overwrite = TRUE) {
    if(!has.vertex.coordinates(graph)){
        stop("Cannot compute edge weights for graph without x,y coordinates.")
    }
    setAttr(graph, "edge", "weight", function(graph) {
        vapply(E(graph), function(edge){
            e <- ends(graph, edge)
            ceiling(euclidean.vertex.distance(graph, e[1], e[2]))
        }, 0)
    }, overwrite)
}

#' @param dist.fun A function that accepts the required vector length as an argument
#' and returns a vector of weights of the given length.
#' @describeIn graphModification Set random edge weights.
#' @export
setRandomEdgeWeights <- function(graph, dist.fun = function(n) ceiling(runif(n, 0, 10)), overwrite = TRUE) {
    setAttr(graph, "edge", "weight", function(graph) {
        dist.fun(ecount(graph))
    }, overwrite)
}

#' @describeIn graphModification Set each edge weight to 1.
#' @export
setUniformEdgeWeights <- function(graph, overwrite = TRUE) {
    setAttr(graph, "edge", "weight", function(graph) {
        rep(1, ecount(graph))
    }, overwrite)
}


#' @describeIn graphModification Initialize an empty predecessor matrix.
#' @export
setEmptyShortestPathPredecessors <- function(graph, overwrite = TRUE) {
    setAttr(graph, "graph", "shortest_path_predecessors", function(graph) {
        n <- vcount(graph)
        mat <- matrix(list(), ncol = n, nrow = n)
        colnames(mat) <- V(graph)$name
        rownames(mat) <- V(graph)$name
        mat
    }, overwrite)
}

#' @param from The graph's source vertex for single-source algorithms.
#' For all-shortest-paths algorithms, \code{FALSE} should be passed.
#' @param to The graph's target vertex for single-source algorithms.
#' For all-shortest-paths algorithms, \code{FALSE} should be passed.
#' @describeIn graphModification Set \code{from} and \code{to} and
#' truncate both \code{min_dists} and \code{shortest_path_predecessors}
#' matrices to the specified source.
#' @export
setRoute <- function(graph, from, to) {
    if (from != FALSE) {
        from <- get.vertex(graph, from)
        graph %<>%
            set_graph_attr("min_dists",
                           graph$min_dists[from, , drop = FALSE]) %>%
            set_graph_attr("shortest_path_predecessors",
                           graph$shortest_path_predecessors[from, , drop = FALSE])
    }
    if (to != FALSE) {
        to <- get.vertex(graph, to)
    }
    graph %>%
        set_graph_attr("from", from) %>%
        set_graph_attr("to", to)
}

#' @describeIn graphModification Initialize each vertex front as \code{val}.
#' @param val the default vertex set value.
#' @export
setVertexSets <- function(graph, val=NA, overwrite = TRUE) {
    setAttr(graph, "vertex", "set", function(graph) {
        rep(val, vcount(graph))
    }, overwrite)
}


#' @describeIn graphModification Set vertex names as A-Z. For graphs with more than
#' 26 vertices, vertices will be numbered 1 to N (as characters).
#' @export
setAlphabeticalVertexNames <- function(graph, overwrite = TRUE) {
    setAttr(graph, "vertex", "name", function(graph) {
        n <- vcount(graph)
        if (n > 26) {
            as.character(1:n)
        } else {
            LETTERS[1:n]
        }
    }, overwrite)
}

#' @describeIn graphModification Permute both vertex and edge ids.
#' This is useful so that e.g. Bellman-Ford cannot exploit a graph's artifical structure.
#'
#' @export
permuteGraph <- function(graph) {
    edges <- as_edgelist(graph)
    edge_attributes <- edge.attributes(graph)
    edge_permut <- sample(nrow(edges))
    graph %<>%
        delete_edges(., seq_len(ecount(.))) %>%
        add_edges(as.vector(t(edges[edge_permut,]))) %>%
        permute(., sample(vcount(.)))
    edge.attributes(graph) <- lapply(edge_attributes, function(attr) attr[edge_permut])
    graph
}
