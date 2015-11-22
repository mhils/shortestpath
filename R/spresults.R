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

#' Check whether an object is a shortest path result object
#'
#' @return \code{TRUE} if its argument is a shortest path result object, \code{FALSE} otherwise.
#' @param x The object to check.
#' @export
is.spresults <- function(x) {
    "spresults" %in% class(x)
}
