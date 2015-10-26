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

graphs = floydWarshall(ig)

floydWarshall = function(ig)
{
  #Create the adjacency matrix with weighted edges 
  #(Assumption: weight of edges has to be in the edge attribute "weight" of the given igraph)
  am = get.adjacency(ig,attr='weight',sparse=FALSE)
  nVertices = length(V(ig))
  
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
  
##Creating the predecessor matrix 
shortest_path_predecessor = matrix(data = 0, nrow = nVertices, ncol = nVertices)
for (i in 1:nVertices) {
  for (j in 1:nVertices) {
    shortest_path_predecessor[i,j] = ifelse(min_dist[i,j] != 0 && min_dist[i,j] != Inf, i,0)
  }
}

#initialization of set vector which includes the vertex attribute "set" (known, font, unkown)
#Initially all vertices have the attribute unknown
set = rep(NA, nVertices) #Hier keine Attribute

graph.attributes(ig)$min_dist <- min_dist
graph.attributes(ig)$shortest_path_predecessor <- shortest_path_predecessor
vertex.attributes(ig)$set <- set

#initialization of a list which contains the igraphs with the additional attributes
result = list(ig)
  
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
          if (min_dist[i,k] + min_dist[k,j] < min_dist[i,j]){
            min_dist[i,j] = min_dist[i,k] + min_dist[k,j]
            shortest_path_predecessor[i,j] = shortest_path_predecessor[k,j]
          }
        }
      }
      #Updating attributes
      graph.attributes(ig)$min_dist <- min_dist
      graph.attributes(ig)$shortest_path_predecessor <- shortest_path_predecessor
      vertex.attributes(ig)$set <- set
      result = c(result,list(ig))
    }  
return(result)
}

#print_path(1,5)
#print_path  = function(i, j) {
  #if (i != j){
    #print_path(i,shortest_path_predecessor[i,j])
  #}
  #print(j)
#}
