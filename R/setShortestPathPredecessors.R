setShortestPathPredecessors <- function(graph, spp.fun, overwrite=TRUE){
  if(overwrite || is.null(graph.attributes(graph)$shortest_path_predecessors)){
    graph %<>% set_graph_attr("shortest_path_predecessors", spp.fun(graph))
  }
  graph
}

setEmptyShortestPathPredecessors <-
  partial(
    setShortestPathPredecessors,
    spp.fun=function(graph){
      n <- length(V(graph))
      mat <- matrix(NA, ncol=n, nrow=n)
      colnames(mat) <- V(graph)$name
      rownames(mat) <- V(graph)$name
      mat
    }
  )
