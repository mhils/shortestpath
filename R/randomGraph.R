
#' Create a random regular graph
#'
#' Generate a random graph with a fixed average vertex degree
#' and at most one cluster.
#' @param k The average degree of each vertex in the graph
#' @inheritParams igraph::sample_k_regular
#' @export
sample_average_k_connected_graph <- function(no.of.nodes, k){
  g <- sample_k_regular(no.of.nodes, k*2)
  maxEdges <- ceiling((no.of.nodes*k) / 2)
  while(length(E(g)) > maxEdges){
    g2 <- delete.edges(g, sample(E(g),1))
    if(no.clusters(g2) == 1){
      g <- g2
    }
  }
  g
}

#' Random Graph Generation
#'
#' This method generates a random graph suitable for exercises.
#'
#' @param euclidean Set to \code{TRUE} if (rounded) euclidean distances should be used.
#' @inheritParams sample_average_k_connected_graph
#' @export
randomGraph <- function(no.of.nodes=12, k=2.5, euclidean=FALSE) {

  if(euclidean == TRUE){
    stop("random euclidean graphs are unimplemented") # nocov
  }

  sample_average_k_connected_graph(no.of.nodes, k) %>%
      makeShortestPathGraph() %>%
      setRandomEdgeWeights()

}
