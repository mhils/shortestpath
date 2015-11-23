getShortestPaths <- function(...) UseMethod("getShortestPaths")

getShortestPaths.spresults <- function(spresults, ...){
    graph <- spresults[[length(spresults)]]
    getShortestPaths(graph, ...)
}

getShortestPaths.spgraph <- function(graph, from=graph$from, to=graph$to) {
    from <- get.vertex(graph, from)
    to <- get.vertex(graph, to)
    if(from == to){
        return(list(
            list(vertices=to, edges=NULL)
        ))
    }
    to_ <- to  # "to" is a reserved word in graph <3
    predecessors <- graph$shortest_path_predecessors[[from$name, to_$name]]


    all <- list()
    # If you iterate over the predecessors directly, you die.
    # (You don't die. But the predecessors loose its type.)
    for(i in seq_along(predecessors)){
        p <- V(graph)[predecessors[i]]
        routes <- lapply(getShortestPaths(graph, from, p), function(x) {
            edge <- E(graph)[p$name %->% to_$name]

            if(length(x$edges) == 0){
                edges <- edge
            } else {
                edges <- c(x$edges, edge)
            }

            list(
                vertices=c(x$vertices,to),
                edges=edges
            )
        })
        all <- c(all, routes)
    }
    all
}
