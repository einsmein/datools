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

#' Replace NA with a user given value in a vector
#'
#' This function replaces all NA's in a vector with a value provided by the user.
#'
#' @param x the vector to replace all NA in
#' @param y the value to replace the NA with
#'
#' @return a new vector with all NA's replaced by y
#' @export
#'
#' @examples
#' naToZero(c(NA, 3:5, NA, NA, 4, 4, 10, NA))
#' naToVal(c(NA, 3:5, NA, NA, 4, 4, 10, NA), 0)
naToVal <- function (x, y=0)
{
  x[is.na(x)] <- y
  return(x)
}
