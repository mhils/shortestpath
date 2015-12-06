spresults <- function(lst=list()){
    for(i in lst){
        if(!is.spgraph(i)){
            stop("spresults graph entry is not an spgraph")
        }
    }
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

#' Check whether an object is a shortest path result object
#'
#' @return \code{TRUE} if its argument is a shortest path result object, \code{FALSE} otherwise.
#' @param x The object to check.
#' @export
is.spresults <- function(x) {
    "spresults" %in% class(x)
}

#' Print graphs to the terminal
#' @param x The graph to print.
#' @param ... Further arguments passed to \code{\link[igraph]{print.igraph}}
#' @export
print.spresults <- function(x, ...) {
    graph <- x[[length(x)]]

    title <- "Shortest Path Results "
    if(graph$from != FALSE || graph$to != FALSE){
        if(graph$from != FALSE) {
            title <- paste0(title, graph$from$name)
        }
        title <- paste0(title,"->")
        if(graph$to != FALSE) {
            title <- paste0(title, graph$to$name)
        }
    } else {
        title <- paste0(title,"[all shortest paths]")
    }

    title <- paste0(title, " (")

    if(graph$from != FALSE && graph$to != FALSE) {
        title <- paste0(
            title,
            "min dist: ",
            graph$min_dists[graph$from$name, graph$to$name],
            ", "
        )
    }

    title <- paste0(title, "steps: ",length(x), ")\r\n+ graph: ")
    cat(title)
    summary(graph, ...)
    invisible(x)
}
