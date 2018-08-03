
#' Standardize a vector of numbers
#'
#' This standardizes a vector of numbers. Standardization rescales the original
#' data into a range between a and b which are given as parameters. They are 0
#' and 1 respectively by default.
#'
#' @param x the vector of numbers to standardize
#' @param a the lower limit of the wanted scale
#' @param b the upper limit of the wanted scale
#'
#' @return the rescaled vector of numbers
#' @export
#'
#' @examples
#' standardize(rnorm(10, 20, 10))
standardize <- function (x, a=0, b=1) {
  if (max(x) - min(x) < 2 * .Machine$double.neg.eps) {
    y <- rep((b - a) / 2 + a, length(x))
    names(y) <- names(x)
    return(y)
  }
  ((x - min(x)) / (max(x) - min(x))) * (b - a) + a
}

#' Normalize a vector of numbers
#'
#' This normalizes a vector of numbers. Normarlization rescales the original
#' data into a range with 0 mean and unit variance.
#'
#' @param x the vector of numbers to normalize
#'
#' @importFrom stats sd
#' @return the normalized vector of numbers
#' @export
#'
#' @examples
#' normalize(rnorm(10, 20, 10))
normalize <- function (x) {
  (x - base::mean(x)) / stats::sd(x)
}
