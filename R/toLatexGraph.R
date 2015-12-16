#' Export spgraphs as TikZ graphs
#' @param x The object to export.
#' @export
toLatexGraph <- function(x) UseMethod("toLatexGraph")

toLatexGraph.spgraph <- function(x){
    graph = x

    head <- paste0(
        "\\begin{tikzpicture}[scale=4]\n",
        "\\tikzstyle{weight}=[midway, fill=white,font=\\sffamily]\n",
        "\\tikzstyle{vertex}=[circle,draw,fill=blue!20!,font=\\sffamily\\large\\bfseries]\n"
    )
    cat(head)

    coords <- layout_(graph, nicely(), normalize())
    coords <- round(coords,1)

    for(i in seq_len(vcount(graph))){
        vertex_name <- V(graph)[[i]]$name
        vertex_line <- paste0(
            "\\node [vertex] (", vertex_name, ") at ",
            "(",coords[i,1],",",coords[i,2],") ",
            "{",vertex_name, "};\n"
        )
        cat(vertex_line)
    }

    cat("\n")

    for(i in seq_len(ecount(graph))){
        edge <- E(graph)[[i]]
        head <- head_of(graph, edge)
        tail <- tail_of(graph, edge)
        direction <- if (is.directed(graph)) { "[->]" } else { "" }
        edge_line <- paste0(
            "\\draw",direction," ",
            "(",head$name,") -- (",tail$name,") ",
            "node [weight] {",edge$weight,"};\n"
        )
        cat(edge_line)
    }
    cat("\\end{tikzpicture}")
}

toLatexGraph.spresults <- function(x){
    toLatexGraph(x[[1]])
}
