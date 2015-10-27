setVertexFronts <- function(graph, fronts.fun, overwrite=TRUE){
  if(overwrite || is.null(vertex.attributes(graph)$set)){
    graph %<>% set_vertex_attr("set", value=fronts.fun(graph))
  }
  graph
}

setEmptyVertexFronts <-
  partial(
    setVertexFronts,
    fronts.fun=function(graph){ rep(NA, length(V(graph))) }
  )
