#' @name shortestpath
#' @import igraph
#' @importFrom magrittr %<>%
#' @importFrom wesanderson wes_palette
#' @importFrom stats runif
#' @importFrom utils capture.output
#' @importFrom utils combn
NULL

#' The shortestpath package
#'
#' The shortestpath package provides a set of functions to execute and visualize algorithms
#' that solve shortest path problems.
#'
#' @section Creating graphs:
#' To create random graphs, which are neither too uniform nor too degenerated,
#' shortestpath's \code{\link{randomGraph}} function can be used. This method generates a random graph
#' with the given number of nodes and average connectivity.
#'
#' All shortest path algorithms also accept regular \link[igraph]{igraph} graph objects as input.
#'
#' @section Algorithms:
#' The package implements the following algorithms:
#' \itemize{
#' \item{Dijkstra (\code{\link{dijkstra}})}
#' \item{A* Search (\code{\link{aStarSearch}})}
#' \item{Bellman-Ford (\code{\link{bellmanFord}})}
#' \item{Floyd-Warshall (\code{\link{floydWarshall}})}
#' }
#'
#' @section Visualization:
#' R's builtin \code{plot()} and \code{print()} functions can be used to
#' visualize the results and intermediate steps of each algorithm.
#' There is a multitude of settings to configure plot visuals (see \code{\link{plot.spgraph}}).
#'
#' Moreover, the package also provides features to export the results to LaTeX tables (\code{\link{toLatexTable}})
#' and TikZ plots (\code{\link{toLatexGraph}}).
#'
#' @section Detailed information:
#' A more detailed documentation of the package in the form of R vignette can be accessed by running \code{browseVignettes("shortestpath")}.
#'
#' See
#' \itemize{
#'  \item{\code{vignette("p01-getting-started", package = "shortestpath")}}
#'  {for an introduction and initial overview,}
#'  \item{\code{vignette("p02-generating-graphs", package = "shortestpath")}}
#'  {for getting to know different functions to generate graphs,}
#'  \item{\code{vignette("p03-algorithms", package = "shortestpath")}}
#'  {for detailed information about each of the algorithm and how they differ,}
#'  \item{\code{vignette("p04-export", package = "shortestpath")}}
#'  {for becoming familiar with the packages's functions to visualize the optimization steps and}
#'  \item{\code{vignette("p05-data-structures", package = "shortestpath")}}
#'  {for learning more details about the structure and functions of spgraph and spresult objects}
#'  }
#' @name shortestpath
NULL

# https://github.com/smbache/magrittr/issues/29
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
