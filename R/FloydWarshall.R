library(igraph)

#' Calculates the shortest path of all pairs of vertices in a graph
#' 
#' 
#' @param graph The \code{igraph} object.
#' @return A list of \code{spgraph} objects. Each \code{spgraph} object contains information about 
#' a certain step in the optimization process representing by its attributes 
#' @import igraph
#' 

floydWarshall = function(graph)
{
  #Create the adjacency matrix with weighted edges 
  #(Assumption: weight of edges has to be in the edge attribute "weight" of the given igraph)
  am = get.adjacency(graph,attr='weight',sparse=FALSE)
  nVertices = length(V(graph))
  
  
  #initialization of the matrix "min_dists"
  #Initialize the solution matrix same as input graph matrix.
  #Or we can say the initial values of shortest distances
  #are based on shortest paths considering no intermediate vertex.
  min_dists = matrix(data = 0, nrow = nVertices, ncol = nVertices)
  for(i in 1:nVertices){
    for(j in 1:nVertices){
      #Replace 0 with INF if there is no edge between the 2 vertices and if not distance to the vertex itself
      min_dists[i,j] = ifelse(am[i,j] > 0 | i == j , am[i,j], Inf) 
    }  
  }
  
  #initialization of the matrix "shortest_path_predecessors"
  ##Creating the predecessor matrix 
  shortest_path_predecessors = matrix(data = 0, nrow = nVertices, ncol = nVertices)
  for (i in 1:nVertices) {
      for (j in 1:nVertices) {
          shortest_path_predecessors[i,j] = ifelse(min_dists[i,j] != 0 && min_dists[i,j] != Inf, i,0)
        }
    }
  
  #initialization of a ShortestPathGraph (spg)
  spg = makeShortestPathGraph(graph, single_source = FALSE)
  graph.attributes(spg)$min_dists <- min_dists
  graph.attributes(spg)$shortest_path_predecessors <- shortest_path_predecessors
  
  #initialization of a list which contains the spg graphs 
  result = list(spg)
  
#Add all vertices one by one to the set of intermediate vertices.
#---> Before start of a iteration, we have shortest distances between all pairs of vertices such that
#the shortest distances consider only the vertices in set {0, 1, 2, .. k-1} as intermediate vertices.
#----> After the end of a iteration, vertex no. k is added to the set of intermediate vertices 
#and the set becomes {0, 1, 2, .. k} 
    for(k in 1:nVertices)
    { 
      #Pick all vertices as source one by one
      for(i in 1:nVertices)
      {
        #Pick all vertices as destination for the above picked source
        for(j in 1:nVertices)
        {
          #If vertex k is on the shortest path from
          #i to j, then update the value of dist[i][j] 
          if (min_dists[i,k] + min_dists[k,j] < min_dists[i,j]){ #Doesnt work with INF anymore
            min_dists[i,j] = min_dists[i,k] + min_dists[k,j]
            shortest_path_predecessors[i,j] = shortest_path_predecessors[k,j]
          }
        }
      }
      #Updating attributes
      graph.attributes(spg)$min_dists <- min_dists
      graph.attributes(spg)$shortest_path_predecessors <- shortest_path_predecessors
      result = c(result,list(spg))
    }  
return(result)
}

