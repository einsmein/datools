#' Find the relationship between the variables in a data set
#'
#' Finds the hirarchical relationship between all variables and connects them in a network such as
#' to optimize the mutual information.
#'
#' @param data the data.frame containing the observations and variables to inspect
#' @param n the number of networks to consider
#'
#' @return a character matrix with columns from and to representing the variables that are related
#' @export
#'
#' @examples
#' data(iris)
#' data(mtcars)
#' library(dplyr)
#' discover_variable_hierarchies(mtcars, 10) %>% as_tibble()
#' discover_variable_hierarchies(iris, 10) %>% as_tibble()
discover_variable_hierarchies<-function(data, n=5){
  stopifnot(inherits(data, c("tbl_df", "tbl", "data.frame")))
  requireNamespace("bnlearn")
  #data(iris)
  #nets <- bn.boot(iris, statistic = function(x) score(x, data=iris), algorithm="hc", R=5)
  nets <- bnlearn::bn.boot(data, statistic = function(x) x, algorithm="hc", R=n)
  myscores <- sapply(1:length(nets), function(x) bnlearn::score(nets[[x]], data=data))
  myorder <- order(myscores, decreasing=TRUE)
  #par(mfrow = c(2, 3))
  #graphviz.compare(nets[[myorder[1]]], nets[[myorder[2]]], nets[[myorder[3]]], nets[[myorder[4]]], nets[[myorder[5]]])
  bnlearn::arcs(nets[[myorder[1]]])
}

#' Find the relationship between the variables in a data set and plot them
#'
#' Finds the hirarchical relationship between all variables and connects them in a network such as
#' to optimize the mutual information. This also compares the n networks found graphically.
#'
#' @param data the data.frame containing the observations and variables to inspect
#' @param n the number of networks to consider and compare
#' @importFrom graphics par
#'
#' @return a character matrix with columns from and to representing the variables that are related
#' @export
#'
#' @examples
#' data(iris)
#' data(mtcars)
#' library(dplyr)
#' discover_and_plot_variable_hierarchies(mtcars, 10) %>% as_tibble()
#' discover_and_plot_variable_hierarchies(iris, 10) %>% as_tibble()
discover_and_plot_variable_hierarchies<-function(data, n=6){
  stopifnot(inherits(data, c("tbl_df", "tbl", "data.frame")))
  requireNamespace("bnlearn")
  requireNamespace("Rgraphviz")
  if(n>=2) k <- n else k <- 2
  nets <- bnlearn::bn.boot(data, statistic = function(x) x, algorithm="hc", R=k)
  myscores <- sapply(1:length(nets), function(x) bnlearn::score(nets[[x]], data=data))
  myorder <- order(myscores, decreasing=TRUE)
  graphics::par(mfrow = c(1, 2))
  bnlearn::graphviz.compare(nets[[myorder[1]]], nets[[myorder[2]]], shape = "ellipse")
  # bnlearn::graphviz.compare(nets[[myorder[1]]], nets[[myorder[2]]], nets[[myorder[3]]],
  #                           nets[[myorder[4]]], nets[[myorder[5]]], nets[[myorder[6]]],
  #                           shape = "ellipse")
  bnlearn::arcs(nets[[myorder[1]]])
}
