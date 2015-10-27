#' @import igraph
setEdgeWeights <- function(graph, weights.fun, overwrite=TRUE){
  if(overwrite || is.null(edge.attributes(graph)$weight)){
    graph <- set_edge_attr(graph, "weight", value=weights.fun(graph))
  }
  graph
}

#' @import igraph
setRandomEdgeWeights <- function(
  graph,
  dist.fun= . %>% runif(0,10) %>% ceiling(),
  ...
){
  setEdgeWeights(graph, weights.fun = function(graph){ dist.fun(length(E(graph))) }, ...)
}

#' @import igraph
#' @importFrom pryr partial
setUniformEdgeWeights <-
  partial(
    setEdgeWeights,
    weights.fun=function(graph){ rep(1, length(E(graph))) }
  )
