#' @import igraph
setVertexFronts <- function(graph, fronts.fun, overwrite=TRUE){
  if(overwrite || is.null(vertex.attributes(graph)$set)){
    graph <- set_vertex_attr(graph, "set", value=fronts.fun(graph))
  }
  graph
}

#' @import igraph
#' @importFrom pryr partial
setEmptyVertexFronts <-
  partial(
    setVertexFronts,
    fronts.fun=function(graph){ rep(NA, length(V(graph))) }
  )
