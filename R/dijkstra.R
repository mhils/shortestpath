#' Dijkstra Algorithm
#'
#' Use Dijkstra's algorithm to solve a shortest path problem.
#'
#' Dijkstra is a single-source algorithm which cannot deal with negative edge weights.
#' Technically, it is a special case of A*-Search (\code{\link{aStarSearch}}) where the heuristics is zero.
#'
#' @examples
#' g <- randomGraph(6)
#' d <- dijkstra(g, "A", "F")
#'
#' plot(d)
#'
#' for(step in d){
#'   print(step$min_dists)
#' }
#'
#'
#' @inheritParams aStarSearch
#' @export
dijkstra <- function (graph, from, to){
    aStarSearch(graph, from, to, function(graph, v1, v2) 0)
}
