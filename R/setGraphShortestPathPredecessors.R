#' @import igraph
setShortestPathPredecessors <- function(graph, spp.fun, overwrite=TRUE){
  if(overwrite || is.null(graph.attributes(graph)$shortest_path_predecessors)){
    graph <- set_graph_attr(graph, "shortest_path_predecessors", spp.fun(graph))
  }
  graph
}

#' @import igraph
#' @importFrom pryr partial
setEmptyShortestPathPredecessors <-
  partial(
    setShortestPathPredecessors,
    spp.fun=function(graph){
      n <- length(V(graph))
      matrix(NA, ncol=n, nrow=n)
    }
  )
