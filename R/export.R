library(igraph)

#' This function creates the LaTeX code out of a given object
#' @param x The \code{spgraph} object
#' @param ... Additional arguments passed to \code{getShortestPaths.spgraph}
#' @export
toLatexTable <- function(x) UseMethod("toLatexTable")

#' @param title Title of the latex table
#' @param includeCommand Additional arguments passed to \code{toLatexTable.spgraph}
toLatexTable.spgraph = function(x,title = "", includeCommand = TRUE){
  spgraph = x
  nodes = V(spgraph)$name
  numCol = ncol(spgraph$min_dists)
  mPred = createPredMatrix(spgraph, nodes, numCol)
  mDist = createDistMatrix(spgraph, nodes, numCol)

  docHeader = paste0(
  "\\begin{table}\n",
  "\\caption{",title,"}\n",
  "\\setlength{\\tabcolsep}{2pt}\n",
  "\\resizebox{\\columnwidth}{!}{\n",
  "\\begin{tabular}{ c *{",numCol,"}{|r@{ }l} }\n",
  collapse = "")

  if (includeCommand == TRUE){
    docHeader = paste0(
    "\\newcommand{\\spp}[2]{\n",
    "#1 & \\textit{\\hspace{-2pt}(#2)} \n",
    "}\n",
    docHeader, collapse = "")
  }
  tableHeader = vapply(1:numCol,function(x) paste0("& \\multicolumn{2}{c|}{", nodes[x] ,"}", collapse=" "),"")
  tableHeader[numCol] = sub("\\{c\\|\\}\\{(.+)\\}$","{c}{\\1} \\\\\\\\\\\\hline \n",tableHeader[numCol])
  tableHeader = c("$\\Rightarrow$",tableHeader)

  tableBody = NULL

  for(i in 1:nrow(spgraph$min_dists)){
    content = paste0(vapply(1:numCol, function(x) paste0("& \\spp{",mDist[i,x],"}{",mPred[i,x],"}",collapse = ""),""), collapse="")
    tableBody = paste(tableBody,paste(nodes[i], content),"\\\\ \n")
  }

  docBottom = paste0(
  "\\end{tabular}\n",
  "}\n",
  "\\end{table}\n",
  collapse ="")

  result = c(docHeader,tableHeader,tableBody,docBottom)
  cat(result)

}

toLatexTable.spresults = function(x){
    steps = x
    for(i in 1:length(steps)){
    if(i == 1){
    toLatexTable.spgraph(steps[[i]],paste("Step",i),TRUE)
    }
    else{
    toLatexTable.spgraph(steps[[i]],paste("Step",i),FALSE)
  }
 }
}

createPredMatrix = function(spgraph, nodes, numCol){
  predecessor = apply(spgraph$shortest_path_predecessors,1,
                      function(x) lapply(x, function(x) ifelse(is.null(x),"--",toString(nodes[x]))))
  mPredecessor = matrix(unlist(predecessor),ncol = numCol, byrow=T)
  mPredecessor
}

createDistMatrix = function(spgraph, nodes, numCol){
  dist = apply(spgraph$min_dists,2,
               function(x) lapply(x,function(x) ifelse(x == Inf,"$\\infty$",x)))

  mDist = matrix(unlist(dist),ncol = numCol, byrow = T)
  mDist
}

#if(is.spresults(data)){
#  lapply(data, function (x) stargazer(as.data.frame(x),title = title, type = "latex", summary = FALSE, align=TRUE,header = FALSE))
#}
#stargazer(as.data.frame(x),title = title, type = "latex", summary = FALSE, align=TRUE,header = FALSE)






