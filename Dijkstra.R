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



#Dijkstra Algorithm
#Parameter: igraph object as ig, starting vertex as From, ending vertex as To
fDijkstra = function(ig,From,To){

  #Create the adjacency matrix with weighted edges 
  #(Assumption: weight of edges has to be in the edge attribute "weight" of the given igraph)
  am = get.adjacency(ig,attr='weight',sparse=FALSE)
  #am = m
  nVertices = dim(am)[1]
  
  #Calculate the number of vertices
  
  #initialization of the distance matrix, 
  #consisting of the min distances to every vertex from starting vertex (defaul distances = inf)
  min_dist = matrix(c(rep(Inf, nVertices)),ncol=1)
  
  #initialization of shortest path tree set
  #keeps track of vertices whose min distance from source is already calculated and finalized
  #Initially, this set is empty.
  sptSet = rep(FALSE, nVertices)
  
  #initialization of the shortest path predecessor in order to keep track of the path
  shortest_path_predecessor = matrix(c(rep(NA, nVertices)),ncol=1)
  
  #initialization of set vector which includes the vertex attribute "set" (known, font, unkown)
  #Initially all vertices have the attribute unknown
  set = rep("unknown", nVertices)
  
      
  #Distance from starting vertex to itself is always 0
  min_dist[From] = 0 
  
  #shortest predecessor from starting vertex to starting vertex is -
  shortest_path_predecessor[From] = "-"
  
  graph.attributes(ig)$min_dist <- min_dist
  graph.attributes(ig)$shortest_path_predecessor <- shortest_path_predecessor
  vertex.attributes(ig)$set <- set

  #initialization of a list which contains the igraphs with the additional attributes
  result = list(ig)
  
  for(step in 1:nVertices){
    #select the vertex with the min distance
    u = minDistance(min_dist, sptSet) 
    
    #update the sptSet with selected vertex in order to track that min distance from source is already calculated
    sptSet[u] = TRUE

    #Evaluate if distances to neighbouring vertices can be updated with a new shortest distance
    for(v in neighbors(ig,u)){
      # Update min_dist[v] only if:
      not_already_in_sptSet = (sptSet[v] == FALSE)# (1)is not in sptSet, 
      smaller_dist = (min_dist[u]+am[u,v] < min_dist[v])# (3)and total weight of path from src to  v through u is smaller than current value of dist[v]
      if(not_already_in_sptSet && smaller_dist){
        min_dist[v] = min_dist[u] + am[u,v]
        shortest_path_predecessor[v] = u
      }
    }
    set[set[] == "front"] = "known"
    set[u] = "front"
    
    graph.attributes(ig)$min_dist <- min_dist
    graph.attributes(ig)$shortest_path_predecessor <- shortest_path_predecessor
    vertex.attributes(ig)$set <- set
    result = c(result,list(ig))
    
    #stop criterion when target vertex is reached
    if(u == To){break}
  }
  return(result)  
}

minDistance = function(min_dist, sptSet){
  min_index = NULL
  min = Inf #max(distances)
  for(i in 1:length(min_dist)){
    if(min_dist[i] <= min && sptSet[i] == FALSE){ #&& dist[i] != 0
      min = min_dist[i]
      min_index = i
    } 
  }
  return(min_index)
}

ig = graph.adjacency(m, weighted=T, mode="undirected")
dj = fDijkstra(ig,1,5)

for(i in 1:length(dj))
{  
  pruef = data.frame(set = get.vertex.attribute(dj[[i]], "set"),
          shortest_path_predecessor = get.graph.attribute(dj[[i]], "shortest_path_predecessor"),
          min_dist = get.graph.attribute(dj[[i]], "min_dist"))
  print(c("Step",i-1))
  print(pruef)
}

#ig = graph.adjacency(m,mode = "undirected",weighted= TRUE)
#el1 = apply(get.edgelist(ig),1,paste, collapse="-")
#el2 = apply(cbind(get.edgelist(ig)[,2],get.edgelist(ig)[,1]),1,paste, collapse="-")
#E(ig)$color <- ifelse(el1 %in% dj$edgeSequence | el2 %in% dj$edgeSequence, "red", "black")
#V(ig)$color <- ifelse(V(ig) == dj$From | V(ig) == dj$To , "red", "grey")
#plot(ig, edge.label=E(ig)$weight)
