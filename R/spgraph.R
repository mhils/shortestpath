#' Coerce to a shortest path graph
#'
#' You usually do not need to call this function directly.
#' Instead, supply an igraph to one of the algorithms, which then outputs suitable spgraph objects.
#' @export
#' @param x Object to coerce
as.spgraph <- function(x) UseMethod("as.spgraph")

#' @describeIn as.spgraph Convert an igraph to an spgraph
as.spgraph.igraph <- function(x) {
    x %>%
        setAlphabeticalVertexNames(overwrite = FALSE) %>%
        setUniformEdgeWeights(overwrite = FALSE) %>%
        setInfiniteMinDists(overwrite = FALSE) %>%
        setEmptyShortestPathPredecessors(overwrite = FALSE) %>%
        setRoute(FALSE, FALSE) %>%
        setVertexCoordinatesFromLayout() %>%
        {
            class(.) <- c("spgraph", class(.))
            .
        }
}

#' Check whether an object is a shortest path graph
#'
#' @return \code{TRUE} if its argument is a shortest path graph, \code{FALSE} otherwise.
#' @param x The object to check.
#' @export
is.spgraph <- function(x) {
    "spgraph" %in% class(x)
}

#' Print graphs to the terminal
#' @param x The graph to print.
#' @param ... Further arguments passed to \code{\link[igraph]{print.igraph}}
#' @export
print.spgraph <- function(x, ...) {
    out <- capture.output(print.igraph(x, ...))
    cat(sub("IGRAPH", "SPGRAPH", out), sep="\n")
    invisible(x)
}

summary.spgraph <- function(object, ...) {
    # summary.igraph is currently not exported.
    out <- capture.output(print.igraph(object, full=F, ...))
    cat(sub("IGRAPH", "SPGRAPH", out), sep="\n")
    invisible(object)
}
