library(stargazer)
library(data.table)
library(plyr)
library(igraph)

#' This function creates a data frame out of the shortest_path_graphs in order to show the optimization steps
#'
#' @param The \code{spgraph} object
#' @return If Dijkstra :The data frame consisting of mininum distance from source node, shortest path predecessor and front.
#'         If FloydWarshall :The data frame consisting of mininum distance between all nodes and their shortest path predecessor.
toDataFrame = function(spGraph){
  nodes = V(spGraph)$name
  if(ncol(spGraph$min_dists) == 1){
    predecessor = lapply(spGraph$shortest_path_predecessors, function(x) ifelse(is.null(x),"-",toString(nodes[x])))
    predecessor = rapply(predecessor,c) 
    table = data.frame(spGraph$min_dists,spGraph$shortest_path_predecessors,V(spGraph)$set)
    colnames(table) <- c(paste("minDist from",colnames(spGraph$min_dists)) , "shortest_Predecessor", "front")
    rownames(table) <- nodes  
  }else if (ncol(spGraph$min_dists) > 1){
    
  }
  return(table)
}

#' This function creates the LaTex code out of the data frame
#'
#' @param The data frame
#' @return Latex table
toLatexTable = function(dataFrame,title = ""){
  stargazer(dataFrame,title = title, type = "latex", summary = FALSE, align=TRUE,header = FALSE)
}