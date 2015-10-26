#' Random Shortest Path Problem Graph Generation
#'
#' @export
randomGraph = function () {
  
  g <- random.graph.game(10,0.5)
  graph <- makeShortestPathGraph(g)
  
  graph
}