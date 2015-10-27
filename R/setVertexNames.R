setVertexNames <- function(graph, names.fun, overwrite=TRUE){
  if(overwrite || is.null(vertex.attributes(graph)$name)){
    n <- length(V(graph))
    graph %<>% set_vertex_attr("name", value=names.fun(graph))
  }
  graph
}

setAlphabeticalVertexNames <-
  partial(
    setVertexNames,
    names.fun=function(graph){
      n <- length(V(graph))
      if(n > 26) {
        1:n
      }
      else {
        LETTERS[1:n]
      }
    }
  )
