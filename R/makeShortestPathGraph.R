#' Convert an igraph into a shortest path graph
#'
#' You usually do not need to call this function directly.
#' Instead, supply an igraph to one of the algorithms, which then outputs suitable spgraph objects.
#'
#' @param graph The \code{igraph} object.
#' @inheritParams setRoute
#' @return The \code{spgraph} object.
#' @export
makeShortestPathGraph <- function(graph, from, to) {
    graph %>%
        setAlphabeticalVertexNames(overwrite = FALSE) %>%
        setUniformEdgeWeights(overwrite = FALSE) %>%
        setInfiniteMinDists(overwrite = FALSE) %>%
        setEmptyShortestPathPredecessors(overwrite = FALSE) %>%
        setRoute(from, to) %>%
        {
            class(.) <- c("spgraph", class(.))
            .
        }
}
