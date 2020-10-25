#' Create a number of scenarios based on a single observation
#'
#' Most common use case is a data.frame full of observations and you want to
#' simulate different values of a few variables while keeping the others fixed.
#' This is important in models with interactions as well as non-linear models.
#' The data.frame return from this function can go directly into a
#' stats::predict call if the model being predicted on supports that interface.
#'
#' @param datapoint containing all variables that a scenarios needs in addition
#' to the ones you want to manipulate
#' @param varvals is a named list where each list item is a vector with all the
#' values you would like to test for the named variable
#' @return a tibble with all the combinations of the values you choose to change
#' @import dplyr tidyr
#' @export
#' @examples
#' scenarios(data.frame(A=0, B=4, C=6), varvals=list(B=c(2,3,6), A=c(1,7,9)))
scenarios <- function(datapoint, varvals) {
    stopifnot(names(varvals) %in% base::colnames(datapoint))
    a <- base::expand.grid(varvals) %>% dplyr::as_tibble()
    tmpdf <- dplyr::as_tibble(datapoint) %>%
        dplyr::mutate(datapoint, count = base::nrow(a)) %>%
        tidyr::uncount(count)
    tmpdf[, base::colnames(a)] <- a[, base::colnames(a)]
    tmpdf
}
