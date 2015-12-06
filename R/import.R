#' @import igraph
#' @importFrom magrittr %<>%
#' @importFrom wesanderson wes_palette
#' @importFrom stats runif
#' @importFrom utils capture.output
NULL

# https://github.com/smbache/magrittr/issues/29
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
