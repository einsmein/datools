#' Replace NA with 0 in a vector
#'
#' This function replaces all NA's in a vector with 0
#'
#' @param x the vector to replace all NA in
#'
#' @return a new vector with all NA's replaced by 0
#' @export
#'
#' @examples
#' naToZero(c(NA, 3:5, NA, NA, 4, 4, 10, NA))
naToZero <- function (x)
{
  x[is.na(x)] <- 0
  return(x)
}
