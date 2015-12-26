#' Convert a list of graphs into an spresults object.
#' @export
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

#' @export
`$.spresults` <- function(x, name) {
    if(name == "first"){
        x[[1]]
    } else if(name == "last") {
        x[[length(x)]]
    } else {
        graph <- x[[length(x)]]
        graph_attr(graph, name)
    }
}

#' @export
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

#' Print spresults to the terminal
#' @param x The results to print.
#' @param ... Further arguments passed to \code{\link[igraph]{print.igraph}}
#' @export
print.spresults <- function(x, ...) {
    graph <- x$last
    single_source <- graph$from != FALSE
    single_target <- graph$to != FALSE

    title <- "Shortest Path Results "
    if(single_source || single_target){
        if(single_source) {
            title <- paste0(title, graph$from$name)
        }
        title <- paste0(title,"->")
        if(single_target) {
            title <- paste0(title, graph$to$name)
        }
    } else {
        title <- paste0(title,"[all shortest paths]")
    }

    title <- paste0(title, " (")

    if(single_source && single_target) {
        title <- paste0(
            title,
            "min dist: ",
            graph$min_dists[graph$from$name, graph$to$name],
            ", "
        )
    }

    title <- paste0(title, "alg.steps: ",length(x), ")\r\n")
    cat(title)

    if(single_source && single_target){
        paths <- getShortestPaths(graph)
        nPaths <- length(paths)
        if(nPaths == 0){
            path <- "(no path found)"
        } else {
            path <- paste0(paths[[1]]$vertices$name, collapse="->")
        }
        if(nPaths > 1){
            path <- paste0(path," (+",(nPaths-1)," alternatives)")
        }
        path <- paste0("+ path: ",path,"\r\n")
        cat(path)
    }
    cat("+ graph: ")
    summary(graph, ...)
    invisible(x)
}
