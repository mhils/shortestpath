library(stargazer)
library(data.table)
library(plyr)
library(igraph)

#' Convert the shortest_path_graphs into a LaTex table in order to show the optimization steps
#'
#' @param list of shortest path.
#' @inheritParams setSingleSource
#' @return The \code{spgraph} object.
#' @export
printTable = function(steps){
  nodes = V(steps[[1]])$name
  steps[[3]]$shortest_path_predecessors[4] = 3
  steps[[4]]$shortest_path_predecessors[4] = list(c(3,2))
  steps[[5]]$shortest_path_predecessors[4] = list(c(3,2))
  latextables = NULL
  for(i in 1:length(steps)){
    cMinDist = get.graph.attribute(steps[[i]],"min_dists")
    cSet = get.vertex.attribute(steps[[i]],"set")
    cPath = get.graph.attribute(steps[[i]],"shortest_path_predecessors")
    #cPath[[4]] = as.integer(c(3,2,3,2,4,3,2)) # Nach Napassung des Codes überflüssig
    cPath = lapply(cPath, function(x) ifelse(is.null(x),"-",toString(nodes[x])))
    cPath = rapply(cPath,c) 
                   
    table = data.frame(minDist = cMinDist, path = cPath, front = cSet)#,check.names = TRUE)
    colnames(table) <- c(paste("minDist from",colnames(cMinDist)) , "path", "front")
    rownames(table) <- nodes
    
    latextables = c(latextables,stargazer(table,title = paste("Step",i), type = "text", summary = FALSE, align=TRUE,header = FALSE))#, add.lines = c("Fixed effects?", "No", "No"))
  }
  cat(latextables, file="test.txt", append=FALSE, sep = "\n")
  
  stargazer(table,title = "Step1", type = "text", summary = FALSE, align=TRUE, out = "test.txt", header = FALSE)#, add.lines = c("Fixed effects?", "No", "No"))
  #stargazer(table, type = "latex", summary = FALSE, align=TRUE, out = "test.txt", header = FALSE)#, add.lines = c("Fixed effects?", "No", "No"))
  
  latextables = stargazer(table, title = "", style = "io",summary = FALSE, align=TRUE)#, add.lines = c("Fixed effects?", "No", "No"))
  
  file = "test6.txt"
  cat(latextables, file="test2.txt", append=TRUE, sep = "\n")

  #cat(test, file="test4.txt")
}

# Nested lists code, an example
# Make a nested list
mylist <- list()
mylist_ <- list()
for(i in 1:5) {
  for(j in 1:5) {
    mylist[[j]] <- i*j
  }
  mylist_[[i]] <- mylist
}

# return values from first part of list
laply(mylist_[[1]], identity)
[1] 1 2 3 4 5

# return all values
laply(mylist_, function(x) laply(x, identity))
1  2  3  4  5
[1,] 1  2  3  4  5
[2,] 2  4  6  8 10
[3,] 3  6  9 12 15
[4,] 4  8 12 16 20
[5,] 5 10 15 20 25

#X <- list(list(a = pi, b = list(c = 1:1)), d = "a test")
#rapply(X, function(x) (x), how = "replace")
#rapply(X, sqrt, classes = "numeric", how = "replace")
#rapply(X, nchar, classes = "character",
#       deflt = as.integer(NA), how = "list")
#rapply(X, nchar, classes = "character",
#       deflt = as.integer(NA), how = "unlist")
#rapply(X, nchar, classes = "character", how = "unlist")
#rapply(X, log, classes = "numeric", how = "replace", base = 2)

#table2 = data.table(minDist = cMinDist, path = cPath)
#table4 = rbind(table,data.table(minDist = cMinDist, path = cPath))
#test = stargazer(table)
#test = cPath[[4]][-(length(cPath[[4]])-1)]
#laply(cPath, identity)
#cPath[[4]][[6]]
#names(cPath[[4]])[[6]]
#test = rapply(cPath, function(x)(print(x)), classes = "ANY",
#      deflt = as.integer(NA), how = "unlist")
#do.call(c, unlist(cPath, recursive=FALSE))

