#' Bellman-Ford Algorithm
#'
#' Use the Bellman-Ford algorithm to calculate the shortest path from a source vertex
#' to a target vertex in a directed, weighted graph
#'
#' The Bellman-Ford algorithm is a single source algorithm which can in
#' contrast to the Dijkstra's and A*-Search algorithms deal with negative edge
#' weights (Note in order to find the right shortest path it is required that
#' no negative-weight cycle exist in the graph).
#' The algorithm automatically detects negative-weight cycles and shows a corresponding error message.
#'
#' Like Dijkstra, the algorithm is based on the principle of the relaxation.
#' Wheres Dijkstra greedily select the closest vertex, the Bellman-Ford algorithm simply relaxes
#' all the edges (#V-1 times). Thus, Bellman-Ford has a runtime of #V * #E.
#'
#'
#' @param graph The \code{igraph} object.
#' @param from Source node
#' @param to Target node
#' @examples
#'   g <- randomGraph(6,euclidean=FALSE)
#'
#'   bf <- bellmanFord(g,"A","F")
#'
#'   plot(bf)
#'
#'   for(step in bf){
#'      print(step$min_dists)
#'   }
#' @export
bellmanFord = function(graph,from,to){
graph %<>%
    as.spgraph() %>%
    setRoute(from,to) %>%
    setVertexSets("unknown")

    steps = list()
    directedGraph = as.directed(graph)
    # Relax all edges |V| times (Normally |V|-1, because  A simple shortest
    # path from src to any other vertex can have at-most |V| - 1 edges)
    # However in order to detect possible negative-weight cycles, |V| times is required
    for(i in 1:(vcount(graph)))
    {
        for (edge in E(directedGraph))
        {
            src = V(graph)[ends(directedGraph,edge)[1,1]]
            dest = V(graph)[ends(directedGraph,edge)[1,2]]
            weight = E(directedGraph)[edge]$weight

            current_dist = graph$min_dists[1,dest]
            dist_over_edge = graph$min_dists[1,src] + weight
            # path over front is better than best known path
            if(graph$min_dists[1,src] != Inf &&
                   dist_over_edge < current_dist )
            {
                #identify negative cycles
                if(i == vcount(graph)){
                    stop("graph has a negative cycle")
                }
                graph$min_dists[1,dest] = dist_over_edge
                graph$shortest_path_predecessors[[1, dest]] <- as.numeric(src)
            }# path over front is as good as the best known path
                else if (dist_over_edge == current_dist){
                # In this case, we still add it as a spp (if not already present)
                if(!(src %in% graph$shortest_path_predecessors[[1, dest]]))
                    {
                    graph$shortest_path_predecessors[[1, dest]] <-
                        c(graph$shortest_path_predecessors[[1, dest]],as.numeric(src))
                    }
                }
            }
        steps <- c(steps, list(graph))
        #stop criterion = if nothing changes to the loop before
        if(i > 1 && all(steps[[i-1]]$min_dists == graph$min_dists)){
            break
        }
    }
    spresults(steps)
}
