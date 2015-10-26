library(igraph)
library(igraphdata)
data(package="igraphdata")
data(karate)


m = matrix(
  c(0,1,1,0,0,
    1,0,0,2,0,
    1,0,0,2,0,
    0,2,2,0,4,
    0,0,0,4,0
  ), ncol=5)

ig = graph.adjacency(m, weighted=T, mode="undirected")

floydWarshall = function(ig)
{
  #Create the adjacency matrix with weighted edges 
  #(Assumption: weight of edges has to be in the edge attribute "weight" of the given igraph)
  am = get.adjacency(ig,attr='weight',sparse=FALSE)
  #am = m
  nVertices = dim(am)[1]
  
  #Initialize the solution matrix same as input graph matrix.
  #Or we can say the initial values of shortest distances
  #are based on shortest paths considering no intermediate vertex.
  min_dist = matrix(data = 0, nrow = nVertices, ncol = nVertices)
  for(i in 1:nVertices){
    for(j in 1:nVertices){
      #Replace 0 with INF if there is no edge between the 2 vertices and if not distance to the vertex itself
      min_dist[i,j] = ifelse(am[i,j] > 0 | i == j , am[i,j], Inf) 
    }  
  }
  
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
        #Pick all vertices as destination for the
        #above picked source
        for(j in 1:nVertices)
        {
          #If vertex k is on the shortest path from
          #i to j, then update the value of dist[i][j]
          if (min_dist[i,k] + min_dist[k,j] < min_dist[i,j]){
            min_dist[i,j] = min_dist[i,k] + min_dist[k,j]
          }
        }
      }
    }
  
  #Print the shortest distance matrix
  print(min_dist)
}