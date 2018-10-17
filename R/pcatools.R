#' Plot original data in PCA space
#'
#' This visualizes the directionality of the original data in Principal
#' Component Analysis (PCA) space.
#'
#' @param mytibble the data frame to operate on which can only contain numeric values
#' @param classes optional class for each observation in the data frame
#' @importFrom stats prcomp
#' @importFrom ggbiplot ggbiplot
#' @return a ggplot2 object representing the PCA plot
#' @export
#'
#' @examples
#' library(datools)
#' plotPCAComponent(iris[,-5], iris$Species) + theme_minimal()
plotPCAComponent <- function(mytibble, classes=NULL) {
  mypca <- stats::prcomp(mytibble, center = TRUE, scale. = TRUE)
  if(all(!missing(classes), length(classes)==nrow(mytibble)))
    ret <- ggbiplot::ggbiplot(mypca, groups = classes, circle = TRUE, ellipse = TRUE)
  else
    ret <- ggbiplot::ggbiplot(mypca, circle = TRUE)
  ret
}
