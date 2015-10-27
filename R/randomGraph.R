#' Random Shortest Path Problem Graph Generation
#'
#' @export
#' @import igraph
randomGraph = function () {

  random.graph.game(10,0.5) %>%
    makeShortestPathGraph() %>%
    setRandomEdgeWeights()

}
