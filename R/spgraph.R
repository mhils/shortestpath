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
        {
            class(.) <- c("spgraph", class(.))
            .
        }
}
