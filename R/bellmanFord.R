#' Calculates the shortest path from a source vertex to a target vertex in a graph
#'
#'
#' @param graph The \code{igraph} object.
#' @param from Source node
#' @param to Target node
#' @export
bellmanFord = function(graph,from,to){
graph %<>%
    as.spgraph() %>%
    setRoute(from,to) %>%
    setVertexSets("unknown")

    steps = list()
    graph = as.directed(graph)
# Step 2: Relax all edges |V| - 1 times. A simple shortest
# path from src to any other vertex can have at-most |V| - 1 edges
    for(i in 1:(vcount(graph)))
    {
        for (edge in E(graph))
        {
            src = V(graph)[ends(graph,edge)[1,1]]
            dest = V(graph)[ends(graph,edge)[1,2]]
            weight = E(graph)[edge]$weight

            current_dist = graph$min_dists[1,dest]
            dist_over_edge = graph$min_dists[1,src] + weight
            if(graph$min_dists[1,src] != Inf &&
                   dist_over_edge < current_dist )
            {   #identify negative cycles

                if(i == vcount(graph)){
                    stop("graph has a negative cycle ")
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



