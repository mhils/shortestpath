#' Plotting of shortest path graphs
#' @return \code{TRUE} if its argument is a shortest path graph, \code{FALSE} otherwise.
#' @export 
is.spgraph <- function(graph){
  "spgraph" %in% class(graph)
}

makeVertexNames <- function(n){ if(n > 26) { 1:n } else { LETTERS[1:n] } }

makeShortestPathGraph <- function(
  graph,
  names = makeVertexNames,
  weights = function(n){ rep(1,n) }, # ceiling(runif(nEdges, 0, 10))
  fronts = function(n){ rep(NA,n) },
  algorithm_type = c("single", "all")
  ){
  nVertex <- length(V(graph))
  nEdges <- length(E(graph))
  
  # Add vertex names
  if(is.null(vertex.attributes(graph)$name)) {
    vertex.attributes(graph)$name <- names(nVertex)
  }
  
  # Add edge weights
  if(is.null(edge.attributes(graph)$weight)) {
    edge.attributes(graph)$weight <- weights(nEdges)
  }
  
  # Add vertex front
  if(is.null(vertex.attributes(graph)$set)) {
    vertex.attributes(graph)$set <- fronts(nVertex)
  }
  
  # Set min_dist
  if(is.null(graph.attributes(graph)$min_dist)) {
    min_dist <- get.adjacency(graph)
    min_dist[min_dist == 0] <- Inf
    
    shortest_path_predecessor <- t(replicate(nrow(min_dist), colnames(min_dist)))
    shortest_path_predecessor[as.logical(min_dist == Inf)] <- NA
    colnames(shortest_path_predecessor) <- rownames(min_dist)
    rownames(shortest_path_predecessor) <- rownames(min_dist)
    
    if(match.arg(algorithm_type) == "single") {
      min_dist <- min_dist[,1,drop=FALSE]
      shortest_path_predecessor <- shortest_path_predecessor[,1,drop=FALSE]
    }
    
    graph.attributes(graph)$min_dist <- min_dist
    graph.attributes(graph)$shortest_path_predecessor <- shortest_path_predecessor
  }
  
  if(is.null(graph.attributes(graph)$shortest_path_predecessor)) {
    stop("While min_dist is provided, shortest_path_predecessor is missing.")
  }
  
  # Set class
  class(graph) <- c("spgraph", class(graph))
  
  graph
}
