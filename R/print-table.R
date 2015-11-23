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
  numCol = ncol(spgraph$min_dists)
  if(numCol == 1){
    predecessor = lapply(spgraph$shortest_path_predecessors, function(x) ifelse(is.null(x),"--",toString(nodes[x])))
    predecessor = rapply(predecessor,c) 
    table = data.frame(spgraph$min_dists,spgraph$shortest_path_predecessors,V(spgraph)$set)
    colnames(table) <- c(paste("minDist from",colnames(spgraph$min_dists)) , "shortest_Predecessor", "front")
    rownames(table) <- nodes  
  }else if (numCol > 1){
    mPred = createPredMatrix(spgraph, nodes, numCol)
    colnames(mPred) = sapply(nodes,function(x) paste(x,".pred",sep=""))
    mDist = createDistMatrix(spgraph, nodes, numCol)
    colnames(mDist) = sapply(nodes,function(x) paste(x,".dist",sep=""))
    m = cbind(mDist,mPred)
    table = as.data.frame(m)
    table = table[,order(names(table))]
    rownames(table) <- nodes
  }
  return(table)
}

as.data.frame.spresults = function(steps){
  as.data.frame(steps[[length(steps)]])  
}

toLatexTable <- function(x,...) UseMethod("toLatexTable")
#' This function creates the LaTex code out of the data frame
#'
#' @param The data frame
#' @return Latex table
toLatexTable.spgraph = function(spgraph,title = ""){
  nodes = V(spgraph)$name
  numCol = ncol(spgraph$min_dists) 
  mPred = createPredMatrix(spgraph, nodes, numCol)
  mDist = createDistMatrix(spgraph, nodes, numCol)

  docHeader = paste("
  \\newcommand{\\sppCost}[1]{#1}
  \\newcommand{\\sppPred}[1]{\\textit{(#1)}}
  \\begin{table}
  \\caption{",title,"}
  \\begin{tabular}{ c *{",numCol,"}{|r@{ }l} }", sep = "")
  tableHeader = vapply(1:numCol,function(x) paste0("& \\multicolumn{2}{c|}{", nodes[x] ,"}", collapse=" "),"")
  tableHeader[numCol] = sub("\\{c\\|\\}\\{(.+)\\}$","{c}{\\1} \\\\\\\\\\\\hline \n",tableHeader[numCol])
  tableBody = NULL
  
  for(i in 1:numCol){
    content = paste0(vapply(1:numCol, function(x) paste0("& \\sppCost{",mDist[i,x],"} & \\sppPred{",mPred[i,x],"}",collapse = ""),""), collapse="")  
    tableBody = paste(tableBody,paste(nodes[i], content),"\\\\ \n") 
  }
  
  docBottom = paste("
  \\end{tabular}
  \\end{table}
  ", sep ="")
  
  result = c(docHeader,tableHeader,tableBody,docBottom)
  cat(result)
     
}

toLatexTable.spresults = function(steps){
  for(i in 1:length(steps)){
    toLatexTable(steps[[i]],paste("Step",i))  
  }
}

createPredMatrix = function(spgraph, nodes, numCol){
  predecessor = apply(spgraph$shortest_path_predecessors,1, 
                      function(x) lapply(x, function(x) ifelse(is.null(x),"--",toString(nodes[x])))) 
  mPredecessor = matrix(unlist(predecessor),ncol = 5, byrow=T)
  mPredecessor
}

createDistMatrix = function(spgraph, nodes, numCol){
  dist = apply(spgraph$min_dists,2, 
               function(x) lapply(x,function(x) ifelse(x == Inf,"\\infty",x)))
  
  mDist = matrix(unlist(dist),ncol = numCol, byrow = T) 
  mDist
}

#if(is.spresults(data)){
#  lapply(data, function (x) stargazer(as.data.frame(x),title = title, type = "latex", summary = FALSE, align=TRUE,header = FALSE))
#} 
#stargazer(as.data.frame(x),title = title, type = "latex", summary = FALSE, align=TRUE,header = FALSE)   
                    





