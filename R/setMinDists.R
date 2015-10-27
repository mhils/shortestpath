setMinDists <- function(graph, min_dists.fun, overwrite=TRUE){
  if(overwrite || is.null(graph.attributes(graph)$min_dists)){
    graph %<>% set_graph_attr("min_dists", min_dists.fun(graph))
  }
  graph
}

setInfiniteMinDists <-
  partial(
    setMinDists,
    min_dists.fun=function(graph){
      n <- length(V(graph))
      mat <- matrix(Inf, ncol=n, nrow=n)
      diag(mat) <- c(0)
      colnames(mat) <- V(graph)$name
      rownames(mat) <- V(graph)$name
      mat
    }
  )
