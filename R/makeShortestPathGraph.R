#' Convert an igraph into a shortest path graph
#'
#' You usually do not need to call this function directly.
#' Instead, supply an igraph to one of the algorithms, which then outputs suitable spgraph objects.
#'
#' @return The \code{spgraph} object.
makeShortestPathGraph <- function(graph, singleSource=TRUE){
  graph %>%
    setAlphabeticalVertexNames(overwrite=FALSE) %>%
    setUniformEdgeWeights(overwrite=FALSE) %>%
    setEmptyVertexFronts(overwrite=FALSE) %>%
    setInfiniteMinDists(overwrite=FALSE) %>%
    setEmptyShortestPathPredecessors(overwrite=FALSE) %>%
    { if(singleSource == TRUE) setSingleSource(.) else . } %>%
    {
      class(.) <- c("spgraph", class(.))
      .
    }
}
