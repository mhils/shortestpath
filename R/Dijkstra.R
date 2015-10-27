library(igraph)

#' Calculates the shortest path from a starting vertex to an ending vertex
#'  
#' @param graph The \code{igraph} object.
#' @param From The starting vertex from which the shortest path need to be calculated.
#' @param To The ending vertex which wants to be reached from the starting vertex by the shortest path.
#' @return A list of \code{spgraph} objects. Each \code{spgraph} object contains information about 
#' a certain step in the optimization process representing by its attributes 
#' @import igraph
#' 

#Parameter: igraph object as graph, starting vertex as From, ending vertex as To
fDijkstra = function(graph,From,To){
  nVertices = length(V(graph))

  #initialization of the adjacency matrix with weighted edges
  #(Assumption: weight of edges has to be in the edge attribute "weight" of the given igraph)
  am = get.adjacency(graph,attr='weight',sparse=FALSE)

  #initialization of a ShortestPathGraph (spg)
  spg = makeShortestPathGraph(graph,single_source = TRUE)

  #initialization of the matrix "min_dists"
  min_dists = get.graph.attribute(spg, "min_dists")
  
  #initialization of the matrix "shortest_path_predecessors"
  shortest_path_predecessors = get.graph.attribute(spg, "shortest_path_predecessors")
  
  #initialization of the matrix "set"
  set = get.vertex.attribute(spg, "set")

  #initialization of shortest path tree set
  #keeps track of vertices whose min distance from source is already calculated and finalized
  #Initially, this set is empty.
  sptSet = rep(FALSE, nVertices)

  #initialization of a list which contains the igraphs with the additional attributes
  result = list(spg)

  for(step in 1:nVertices){
    #select the vertex with the min distance
    u = minDistance(min_dists, sptSet)

    #update the sptSet with selected vertex in order to track that min distance from source is already calculated
    sptSet[u] = TRUE

    #Evaluate if distances to neighbouring vertices can be updated with a new shortest distance
    for(v in neighbors(graph,u)){
      # Update min_dists[v] only if:
      # (1)is not in sptSet,
      not_already_in_sptSet = (sptSet[v] == FALSE)
      # (3)and total weight of path from src to  v through u is smaller than current value of dist[v]
      total_dist_smaller = (min_dists[u]+am[u,v] < min_dists[v])
      if(not_already_in_sptSet && total_dist_smaller){
        min_dists[v] = min_dists[u] + am[u,v]
        shortest_path_predecessors[v] = u
      }
    }
    set[set[] == "front"] = "known"
    set[u] = "front"
    
    #Change the attributes of the spg object to the new values
    graph.attributes(spg)$min_dists <- min_dists
    graph.attributes(spg)$shortest_path_predecessors <- shortest_path_predecessors
    vertex.attributes(spg)$set <- set
    
    #Add the spg object to the list
    result = c(result,list(spg))

    #stop criterion when target vertex is reached
    if(u == To){break}
  }
  return(result)
}

minDistance = function(min_dists, sptSet){
  min_index = NULL
  min = Inf #max(distances)
  for(i in 1:length(min_dists)){
    if(min_dists[i] <= min && sptSet[i] == FALSE){ #&& dist[i] != 0
      min = min_dists[i]
      min_index = i
    }
  }
  return(min_index)
}

