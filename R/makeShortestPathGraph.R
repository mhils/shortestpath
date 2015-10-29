#' Convert an igraph into a shortest path graph
#'
#' You usually do not need to call this function directly.
#' Instead, supply an igraph to one of the algorithms, which then outputs suitable spgraph objects.
#'
#' @param graph The \code{igraph} object.
#' @inheritParams setSingleSource
#' @return The \code{spgraph} object.
#' @export
makeShortestPathGraph <- function(graph, source = TRUE) {
    graph %>%
        setAlphabeticalVertexNames(overwrite = FALSE) %>%
        setUniformEdgeWeights(overwrite = FALSE) %>%
        setUniformVertexFronts(overwrite = FALSE) %>%
        setInfiniteMinDists(overwrite = FALSE) %>%
        setEmptyShortestPathPredecessors(overwrite = FALSE) %>%
        setSingleSource(source) %>%
        {
        class(.) <- c("spgraph", class(.))
        .
        }
}
