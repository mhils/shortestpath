context("Dijkstra")

library(igraph)

line = matrix(
  c(0,1,Inf,Inf,Inf,
    1,0,2,Inf,Inf,
    Inf,2,0,3,Inf,
    Inf,Inf,3,0,4,
    Inf,Inf,Inf,4,0),
  ncol = 5, byrow = TRUE
)
lgraph = graph.adjacency(line, weighted=T, mode="undirected")

diamond = matrix(
  c(0,1,1,0,0,
    1,0,0,2,0,
    1,0,0,2,0,
    0,2,2,0,4,
    0,0,0,4,0
  ), ncol=5,byrow = TRUE)
dgraph = graph.adjacency(line, weighted=T, mode="undirected")

test_that("dijkstra calculates shortest Path right on a graph represented by a line", { 
  spgs = dijkstra(lgraph,1,5) 
  expect_equal(get.graph.attribute(spgs[[6]],"min_dists"), c(0,1,3,6,10) )
})

test_that("dijkstra calculates predecessors right on a graph represented by a line", {
  spgs = dijkstra(lgraph,1,5) 
  expect_equal(get.graph.attribute(spgs[[6]],"shortest_path_predecessors"),c(NA,1,2,3,4) )
})

test_that("dijkstra needs right amount of steps on a graph represented by a line ", {
  spgs = dijkstra(lgraph,1,5) 
  expect_equal(length(spgs),6)
})


  
