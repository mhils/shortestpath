#' Plotting of shortest path graphs
#' @return \code{TRUE} if its argument is a shortest path graph, \code{FALSE} otherwise.
#' @export 
is.spgraph <- function(graph){
  "is.spgraph" %in% class(graph)
}
