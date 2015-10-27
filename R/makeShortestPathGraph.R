#' Convert an igraph into a shortest path graph
#'
#' You usually do not need to call this function directly.
#' Instead, supply an igraph to one of the algorithms, which then outputs suitable spgraph objects.
#'
#' @param graph The \code{igraph} object.
#' @param singleSource If \code{TRUE}, min_dists and
#' @return The \code{spgraph} object.
#' @export
makeShortestPathGraph <- function(graph, singleSource = TRUE) {
    graph %>%
        setAlphabeticalVertexNames(overwrite = FALSE) %>%
        setUniformEdgeWeights(overwrite = FALSE) %>%
        setEmptyVertexFronts(overwrite = FALSE) %>%
        setInfiniteMinDists(overwrite = FALSE) %>%
        setEmptyShortestPathPredecessors(overwrite = FALSE) %>%
        {
            if (singleSource != FALSE)
                setSingleSource(., singleSource)
            else
                .
        } %>%
        {
        class(.) <- c("spgraph", class(.))
        .
        }
}
