#' Graph Modification
#'
#' shortestpath provides various graph modification functions that are used internally.
#'
#' @examples
#'
#' library(igraph)
#'
#' g <- random.graph.game(10, 0.5) %>%
#'  setAlphabeticalVertexNames %>%
#'  setRandomEdgeWeights()
#'
#' plot(g)
#' E(g)$weight
#'
#'
#' @param graph The original graph object. Will not be modified.
#' @param overwrite If \code{FALSE}, graphs with an existing attribute will not
#' be updated.
#' @return The updated graph object.
#' @name Graph Modification
#' @rdname graph-modification
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

#' @describeIn graph-modification Set the minimum distance matrix to infinity.
#' @export
setInfiniteMinDists <- function(graph, overwrite = TRUE) {
    setAttr(graph, "graph", "min_dists", function(graph) {
        n <- length(V(graph))
        mat <- matrix(Inf, ncol = n, nrow = n)
        diag(mat) <- c(0)
        colnames(mat) <- V(graph)$name
        rownames(mat) <- V(graph)$name
        mat
    }, overwrite)
}

#' @param dist.fun A function that accepts the required vector length as an argument
#' and returns a vector of weights of the given length.
#' @describeIn graph-modification Set random edge weights.
#' @export
setRandomEdgeWeights <- function(graph, dist.fun = function(n) ceiling(runif(n, 0, 10)), overwrite = TRUE) {
    setAttr(graph, "edge", "weight", function(graph) {
        dist.fun(length(E(graph)))
    }, overwrite)
}

#' @describeIn graph-modification Set each edge weight to 1.
#' @export
setUniformEdgeWeights <- function(graph, overwrite = TRUE) {
    setAttr(graph, "edge", "weight", function(graph) {
        rep(1, length(E(graph)))
    }, overwrite)
}


#' @describeIn graph-modification Initialize an empty predecessor matrix.
#' @export
setEmptyShortestPathPredecessors <- function(graph, overwrite = TRUE) {
    setAttr(graph, "graph", "shortest_path_predecessors", function(graph) {
        n <- length(V(graph))
        mat <- matrix(NA, ncol = n, nrow = n)
        colnames(mat) <- V(graph)$name
        rownames(mat) <- V(graph)$name
        mat
    }, overwrite)
}

#' @param source The graph's source vertex for single-source algorithms.
#' @describeIn graph-modification Truncate \code{min_dists} and
#' \code{shortest_path_predecessors} matrices to the selected column.
#' @export
setSingleSource <- function(graph, source = FALSE) {
    if (source != FALSE) {
        if (source == TRUE) {
            source <- V(graph)[1]
        }
        graph %<>%
          set_graph_attr("min_dists",
                         graph.attributes(graph)$min_dists[, source, drop = FALSE]) %>%
          set_graph_attr("shortest_path_predecessors",
                         graph.attributes(graph)$shortest_path_predecessors[, source, drop = FALSE])
    }
    graph
}

#' @describeIn graph-modification Initialize each vertex front as \code{NA}.
#' @export
setEmptyVertexFronts <- function(graph, overwrite = TRUE) {
    setAttr(graph, "vertex", "set", function(graph) {
        rep(NA, length(V(graph)))
    }, overwrite)
}


#' @describeIn graph-modification Set vertex names as A-Z. For graphs with more than
#' 26 vertices, vertices will be numbered 1 to N.
#' @export
setAlphabeticalVertexNames <- function(graph, overwrite = TRUE) {
    setAttr(graph, "vertex", "name", function(graph) {
        n <- length(V(graph))
        if (n > 26) {
            1:n
        } else {
            LETTERS[1:n]
        }
    }, overwrite)
}
