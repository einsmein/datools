
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

#' Split a vector of indices into buckets
#'
#' Splits any index vector into a list of index vectors where each vector is of
#' length bucketsize exept for the last one which contains the remainder. The
#' remainder is defined as length(x)%%bucketsize. It works with all kind of
#' discrete ranges. Negative to positive.
#'
#' @param x the indices to split into buckets
#' @param bucketsize the size each bucket should have
#'
#' @return a list of indices of length bucketsize
#' @export
#'
#' @examples
#' rangeToBuckets(1:nrow(mtcars), 9)
#' rangeToBuckets(-2:3, 4)
rangeToBuckets <- function(x, bucketsize=10){
  if(length(x) < bucketsize) return(list(x))
  if(bucketsize < 1) return(list(x))
  nbuckets <- length(x)%/%bucketsize
  remainder <- length(x)%%bucketsize
  inds <- list()
  for(i in 1:nbuckets) inds[[i]] <- x[((i-1)*bucketsize+1):(i*bucketsize)]
  if(remainder > 0) inds[[nbuckets+1]] <- x[(nbuckets*bucketsize+1):length(x)]
  inds
}

