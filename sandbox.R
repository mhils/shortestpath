library(igraph)
library(igraphdata)

# List R data sets 
data(package="igraphdata")

data(karate)

distance_table(karate)
mean_distance(karate)
distances(karate)
plot(karate)

m = matrix(
  c(0,1,1,0,0,
    1,0,0,2,0,
    1,0,0,2,0,
    0,2,2,0,4,
    0,0,0,4,0
  ), ncol=5)
?graph.adjacency
g = graph.adjacency(m, weighted=T, mode="undirected")
plot(g)

min_dist= matrix(c(0,Inf,Inf,Inf,Inf),ncol=1)
graph.attributes(g)$min_dist <- min_dist
shortest_path_predecessor = matrix(c(NA,NA,NA,NA,NA),ncol=1)
graph.attributes(g)$shortest_path_predecessor <- shortest_path_predecessor

vertex_states = as.factor(c("known","front","unknown"))

vertex.attributes(g)$color <- c("red","black","black","black","black")

plot.igraph(g)


d = as_adjacency_matrix(g, attr="weight")
d
d[,1]
plot.igraph(g)

