context("FloydWarshall")

test_that("FloydWarshall runs without errors", {
  
  graph <- randomGraph(n=20,euclidean=TRUE)
  floydWarshall(graph)
})

test_that("FloydWarshall finds the minimal distances", {
  
  test_configurations = list(
    list(n=2, k=1),
    list(n=4, k=2*3/4),
    list(n=4, k=2),
    list(n=4, k=3),
    list(n=20, k=2.5)
  )
  
  for(args in test_configurations){  
    graph <- randomGraph(no.of.nodes=args$n, k=args$k)
    floydwarshall <- floydWarshall(graph)
    last_step <- floydwarshall[[length(floydwarshall)]]
    reference_value <- distances(graph)
    expect_equal(last_step$min_dists, reference_value)
  }
})

test_that("FloydWarshall produces valid shortest_path_predecessors", {
  ring <- make_ring(4)
  E(ring)$weight = c(1,2,2,1)
  floydWarshall <- floydWarshall(ring)
  spp <- floydWarshall[[length(floydWarshall)]]$shortest_path_predecessors
  ref <- matrix(list(), ncol=4, nrow=4)
  ref[[1,2]] <- V(ring)[[2]]
  ref[[1,3]] <- c(V(ring)[[2]],V(ring)[[4]])
  ref[[1,4]] <- V(ring)[[4]]
  ref[[2,1]] <- V(ring)[[1]]
  ref[[2,3]] <- V(ring)[[3]]
  ref[[2,4]] <- V(ring)[[1]]
  ref[[3,1]] <- c(V(ring)[[2]],V(ring)[[4]])
  ref[[3,2]] <- V(ring)[[2]]
  ref[[3,4]] <- V(ring)[[4]]
  ref[[4,1]] <- V(ring)[[1]]
  ref[[4,2]] <- V(ring)[[1]]
  ref[[4,3]] <- V(ring)[[3]]
  
  expect_equivalent(unlist(spp), unlist(ref))
})



