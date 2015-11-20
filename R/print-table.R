library(stargazer)
library(data.table)
library(plyr)
library(igraph)

#' This function creates a data frame out of the shortest_path_graphs in order to show the optimization steps
#'
#' @param \code{spgraph} object
#' @return If Dijkstra :The data frame consisting of mininum distance from source node, shortest path predecessor and front.
#'         If FloydWarshall :The data frame consisting of mininum distance between all nodes and their shortest path predecessor.
as.data.frame.spgraph = function(spgraph){
  nodes = V(spgraph)$name
  if(ncol(spgraph$min_dists) == 1){
    predecessor = lapply(spgraph$shortest_path_predecessors, function(x) ifelse(is.null(x),"-",toString(nodes[x])))
    predecessor = rapply(predecessor,c) 
    table = data.frame(spgraph$min_dists,spgraph$shortest_path_predecessors,V(spgraph)$set)
    colnames(table) <- c(paste("minDist from",colnames(spgraph$min_dists)) , "shortest_Predecessor", "front")
    rownames(table) <- nodes  
  }else if (ncol(spgraph$min_dists) > 1){
    
  }
  return(table)
}

#' This function creates the LaTex code out of the data frame
#'
#' @param The data frame
#' @return Latex table
toLatexTable = function(data,title = ""){
  if(class(data) == "list"){
    lapply(data, function (x) stargazer(as.data.frame(x),title = title, type = "latex", summary = FALSE, align=TRUE,header = FALSE))
  } 
  stargazer(as.data.frame(x),title = title, type = "latex", summary = FALSE, align=TRUE,header = FALSE)   
}