#' @import igraph
setSingleSource <- function(graph){
  graph %>%
    set_graph_attr("min_dists",
                   graph.attributes(graph)$min_dists[,1,drop=FALSE]) %>%
    set_graph_attr("shortest_path_predecessors",
                   graph.attributes(graph)$shortest_path_predecessors[,1,drop=FALSE])
}
