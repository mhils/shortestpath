makeEdgeWeights <- function(graph){ 
  rep(1, length(E(graph))) # alternative: ceiling(runif(nEdges, 0, 10))
} 
makeVertexNames <- function(graph){
  n = length(V(graph))
  if(n > 26) { 
    1:n 
  } 
  else { 
    LETTERS[1:n] 
  } 
}
makeVertexFronts <- function(graph){ 
  rep(NA, length(V(graph))) 
}
makeGraphMinDists <- function(graph) {
  n = length(V(graph))
  m = matrix(Inf,ncol=n,nrow=n)
  diag(m) <- c(0)
  m
}
makeGraphShortestPathPredecessors <- function(graph) {
  n = length(V(graph))
  matrix(NA,ncol=n,nrow=n)
}

#' Convert an igraph into a shortest path graph
#' 
#' You usually do not need to call this function directly.
#' Instead, supply an igraph to one of the algorithms, which then outputs suitable spgraph objects.
#' 
#' @param graph The \code{igraph} object.
#' @param edge.weights A factory function returning edge weights. Uniform weight by default.
#' @param vertex.names A factory function returning vertex names. A-Z by default.
#' @param vertex.fronts A factory function returning the front each vertex is in. NA by default.
#' @param graph.min_dists The minimum distance matrix. Filled with Inf (and 0 on the diagonal) by default.
#' @param graph.shortest_path_predecessors The shortest path predecessor information. NA by default.
#' @param single_source if \code{TRUE}, \code{min_dists} and \code{shortest_path_predecessors} will be
#' cut off to a single column.
#' @return The \code{spgraph} object.
#' @import igraph
makeShortestPathGraph <- function(
  graph,
  edge.weights = makeEdgeWeights,
  vertex.names = makeVertexNames,
  vertex.fronts = makeVertexFronts,
  graph.min_dists = makeGraphMinDists,
  graph.shortest_path_predecessors=makeGraphShortestPathPredecessors,
  single_source=FALSE
){
  
  # Add vertex names
  if(is.null(vertex.attributes(graph)$name)) {
    vertex.attributes(graph)$name <- vertex.names(graph)
  }
  
  # Add edge weights
  if(is.null(edge.attributes(graph)$weight)) {
    edge.attributes(graph)$weight <- edge.weights(graph)
  }
  
  # Add vertex front
  if(is.null(vertex.attributes(graph)$set)) {
    vertex.attributes(graph)$set <- vertex.fronts(graph)
  }
  
  # Set min_dist and shortest path predecessor
  if(is.null(graph.attributes(graph)$min_dists)) {
    graph.attributes(graph)$min_dists <- graph.min_dists(graph)
  }
  if(is.null(graph.attributes(graph)$shortest_path_predecessors)) {
    graph.attributes(graph)$shortest_path_predecessors <- graph.shortest_path_predecessors(graph)
  }
  
  # Cut off additional columns if single_source is true
  if(single_source){
    graph.attributes(graph)$min_dists <- 
      graph.attributes(graph)$min_dists[,1,drop=FALSE]
    graph.attributes(graph)$shortest_path_predecessors <- 
      graph.attributes(graph)$shortest_path_predecessors[,1,drop=FALSE]
  }
  
  # Set class
  class(graph) <- c("spgraph", class(graph))
  
  graph
}