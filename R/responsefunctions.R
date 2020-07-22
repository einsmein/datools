

tf_sigmoid <- function(x, a = 1) 1 / (1 + exp(-a * x))

tf_swish <- function(x, a = 1) x / (1 + exp(-a * x))

tf_tanh <- function(x, a = 1) tanh(a * x)

tf_fisk <- function(x, a = 1, b = 1) 1 / (1 + (x / a)^(-b))

#' The Dagum transformation function
#'
#' An implementation of the CDF for the Dagum distribution.
#'
#' @param x value to transform
#' @param a the scale parameter
#' @param b the first shape parameter
#' @param p the second shape parameter
#'
#' @return x transformed
#' @importFrom ggplot2 ggplot aes stat_function
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x = c(0, 4)), aes(x)) +
#'   stat_function(fun = function(x) tf_dagum(x, 1, 2, 2))
tf_dagum <- function(x, a = 1, b = 1, p = 1) (1 + (x / a)^(-b))^(-p)

#' The ReLu transformation function
#'
#' An implementation of the ReLu transformation function
#'
#' @param x value to transform
#' @importFrom stats rnorm sd
#' @return x transformed
#' @importFrom ggplot2 ggplot aes stat_function
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x = c(0, 4)), aes(x)) +
#'   stat_function(fun = function(x) tf_relu(x))
tf_relu <- function(x) sapply(x, max, 0)

#' The Noise ReLu transformation function
#'
#' An implementation of the Noisy ReLu transformation function
#'
#' @param x value to transform
#' @importFrom stats rnorm sd
#' @return x transformed
#' @importFrom ggplot2 ggplot aes stat_function
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x = c(0, 4)), aes(x)) +
#'   stat_function(fun = function(x) tf_relunoise(x))
#' \dontrun{
#' library(microbenchmark)
#' microbenchmark::microbenchmark(tf_relu(1:100), tf_softplus(1:100),
#'   tf_dagum(1:100), tf_sigmoid(1:100),
#'   tf_fisk(1:100), tf_relunoise(1:100),
#'   times = 10000
#' )
#' }
tf_relunoise <- function(x) {
  mysd <- 0.05 * sd(x)
  sapply(x, function(x) max(x + rnorm(1, 0, mysd), 0))
}

#' The Softplus ReLu transformation function
#'
#' An implementation of the Softplus ReLu transformation function
#'
#' @param x value to transform
#' @importFrom stats rnorm sd
#' @return x transformed
#' @importFrom ggplot2 ggplot aes stat_function
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(data.frame(x = c(0, 4)), aes(x)) +
#'   stat_function(fun = function(x) tf_softplus(x))
tf_softplus <- function(x) log(1 + exp(x))
