#' Plotting of shortest path graphs
#' 
#' @param graph The graph object.
#' @return \code{TRUE} if its argument is a shortest path graph, \code{FALSE} otherwise.
#' @export 
is.spgraph <- function(graph) {
    "spgraph" %in% class(graph)
} 
