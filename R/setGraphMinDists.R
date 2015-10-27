#' @import igraph
setGraphMinDists <- function(graph, min_dists.fun, overwrite=TRUE){
  if(overwrite || is.null(graph.attributes(graph)$min_dists)){
    graph <- set_graph_attr(graph, "min_dists", min_dists.fun(graph))
  }
  graph
}

#' @import igraph
#' @importFrom pryr partial
setInfiniteMinDists <-
  partial(
    setGraphMinDists,
    min_dists.fun=function(graph){
      n <- length(V(graph))
      mat <- matrix(Inf, ncol=n, nrow=n)
      diag(mat) <- c(0)
      mat
    }
  )
