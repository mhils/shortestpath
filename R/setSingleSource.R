setSingleSource <- function(graph, source=FALSE){
  if(source != FALSE){
    if(source == TRUE){
      source <- V(graph)[1]
    }
    graph %<>%
      set_graph_attr("min_dists",
                     graph.attributes(graph)$min_dists[,source,drop=FALSE]) %>%
      set_graph_attr("shortest_path_predecessors",
                     graph.attributes(graph)$shortest_path_predecessors[,source,drop=FALSE])
  }
  graph

}
