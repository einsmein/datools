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
#' if(requireNamespace("Rgraphviz")){
#'   discover_and_plot_variable_hierarchies(mtcars, 10) %>% as_tibble()
#'   discover_and_plot_variable_hierarchies(iris, 10) %>% as_tibble()
#' }
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

#' Find the relationship between the variables in a data set and fit that model
#'
#' Finds the hirarchical relationship between all variables and connects them in a network such as
#' to optimize the mutual information. This just fits the network selected by HillClimbing using a
#' Bayesian information criteria (BIC).
#'
#' @param data the data.frame containing the observations and variables to inspect
#'
#' @return a fitted architecture
#' @export
#'
#' @examples
#' data(mtcars)
#' library(dplyr)
#' myfit <- discover_hierarchy_and_fit(mtcars)
#' if(requireNamespace("Rgraphviz")) bnlearn::graphviz.plot(myfit)
discover_hierarchy_and_fit<-function(data){
  stopifnot(inherits(data, c("tbl_df", "tbl", "data.frame")))
  requireNamespace("bnlearn")
  mystruct <- bnlearn::hc(data)
  myfit <- bnlearn::bn.fit(mystruct, data)
  return(myfit)
}


#' Evolve a hierarchy among variables by hill climbing
#'
#' This method starts by evolving a graph relationship between all variables and
#' afterwards fits that graph to your dataset. It can operate with whitelists
#' and blacklists to enforce and forbid relationships respectively. The scores
#' in the plot are standardized between 0 and 100. The sign of the strength is
#' highlighted by color.
#'
#' @param data the dataset to discover the relationship on. Should be given as a
#' data.frame or tibble.
#' @param blacklist the relationships to forbid in the graph. This defaults to
#' NULL which means we forbid nothing.
#' @param whitelist the relationships to enforce in the graph. This defaults to
#' NULL which means we enforce nothing.
#' @param signcolor a boolean for whether you want the sign to be colored in the
#' Plot
#' @param ... arguments passed to the hillclimbing algorithm in bnlearn.
#' @return  a Ragraph S4 object
#' @importFrom methods new
#' @export
#' @examples
#' data(mtcars)
#' library(Rgraphviz)
#' library(bnlearn)
#' evolve_hierarchy_and_plot(mtcars)
#' blacklist <- data.frame(from=c("cyl", "cyl", "cyl"), to=c("drat", "vs", "mpg"))
#' evolve_hierarchy_and_plot(mtcars, blacklist=blacklist)
evolve_hierarchy_and_plot<-function(data, blacklist=NULL, whitelist=NULL, signcolor=TRUE, ...) {
    mygraph <- bnlearn::hc(data, whitelist=whitelist, blacklist=blacklist, ...)
    mybnfit <- bnlearn::bn.fit(mygraph, data=data)
    # Plot it
    myarcs <- bnlearn::arcs(mygraph)
    myarcs <- myarcs %>% dplyr::as_tibble()
    nAttrs <- list()
    eAttrs <- list()
    attrs <- list(node=list(fontsize=40,
                            fillcolor="#DBE0DE",
                            fontcolor="black"),
                  edge=list(fontsize=40,
                            color="#009FE3"),
                  graph=list(rankdir="LR"))
    rEG <- new("graphNEL", nodes=unique(c(myarcs[["from"]], myarcs[["to"]])),
               edgemode="directed")
    strengths <- sapply(1:nrow(myarcs), function(x) check_sign(mybnfit, myarcs$from[x], myarcs$to[x]))
    strengths <- round(standardize(abs(strengths))*100, 1)
    rEG <- graph::addEdge(myarcs$from, myarcs$to, rEG, strengths)
    ew <- as.character(unlist(graph::edgeWeights(rEG)))
    ew <- ew[setdiff(seq(along=ew), Rgraphviz::removedEdges(rEG))]
    names(ew) <- graph::edgeNames(rEG)
    eAttrs$label <- ew
    encodecolor <- function(x){
        from <- myarcs$from[x]
        to <- myarcs$to[x]
        val <- check_sign(mybnfit, from, to)
        myname <- paste0(from, "~", to)
        if(val >0) ret <- "green"
        else ret <- "red"
        names(ret) <- myname
        ret
    }
    #ec <- c("Buzz~Attention"="green")
    ec <- sapply(1:nrow(myarcs), encodecolor)
    if(signcolor) eAttrs$color <- ec
    Rgraphviz::plot(rEG, edgeAttrs=eAttrs, nodeAttrs=nAttrs, attrs=attrs)
}

check_sign <- function(bnfit, from, to) coef(bnfit[[to]])[[from]]
