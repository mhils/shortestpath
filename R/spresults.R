spresults <- function(lst=list()){
    if(!("spresults" %in% class(lst)))
        class(lst) <- c("spresults", class(lst))
    lst
}

`$.spresults` <- function(x, name) {
    graph <- x[[length(x)]]
    graph_attr(graph, name)
}

`$<-.spresults` <- function(x, name, value) {
    stop("Cannot modify spresults objects - please work with the underlying spgraphs directly.")
}
