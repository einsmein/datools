#' Encode a vector into a one hot data.frame
#'
#' This function takes a vector of items and onehot encodes them into a data.frame
#' which in reality is a tibble. Every unique value of the vector get converted into
#' a column and the value is 1 for that column if the row number matches the position
#' in the original vector. See the examples for details.
#'
#' @param x the vector to convert to a one hot
#' @param f a function to apply to x before encoding which defaults to identity
#' @importFrom dplyr mutate select everything tibble
#' @importFrom reshape2 dcast
#' @return the one hot encoded data.frame with the original data given in a Data column
#' @export
#'
#' @examples
#' library(lubridate)
#' oneHotEncoder(
#'   x = seq(as.Date("2017-01-01"), by = "day", length.out = 10),
#'   f = function(x) wday(x, label = TRUE)
#' )
#' oneHotEncoder(x = wday(seq(as.Date("2017-10-07"), by = "days", length.out = 10), label = TRUE))
oneHotEncoder <- function(x, f = function(y) y) {
  stopifnot(all(length(x) > 1, is.function(f)))
  Data <- NULL
  tibble(Data = seq_along(x), Value = f(x)) %>%
    reshape2::dcast(Data ~ Value, value.var = "Value", fun.aggregate = length) %>%
    dplyr::mutate(Data = x) # %>% dplyr::select(Data, everything())
}

#' Encode a vector into a one hot tibble
#'
#' This function takes a vector of items and onehot encodes them into a tibble
#' which in reality is a tibble. Every unique value of the vector get converted into
#' a column and the value is 1 for that column if the row number matches the position
#' in the original vector. See the examples for details.
#'
#' @param x the vector to convert to a one hot
#' @param f a function to apply to x before encoding which defaults to identity
#' @importFrom dplyr mutate select everything tibble
#' @return the one hot encoded tibble with the original data given in a Data column
#' @export
#'
#' @examples
#' library(lubridate)
#' oneHotEncoder2(
#'   x = seq(as.Date("2017-01-01"), by = "day", length.out = 10),
#'   f = function(x) wday(x, label = TRUE)
#' )
#' oneHotEncoder2(x = wday(seq(as.Date("2017-10-07"), by = "days", length.out = 10), label = TRUE))
oneHotEncoder2 <- function(x, f = function(y) y) {
  stopifnot(all(length(x) > 1, is.function(f)))
  Category <- Data <- Value <- NULL
  tibble(Category = f(x), Data = seq_along(x), Value = 1) %>%
    tidyr::spread(Category, Value, fill = 0) %>%
    mutate(Data = x)
}

#' Lag variables in data nlags
#'
#' This function uses an existing dataset in the shape of a tibble and then
#' lags each variable(column) in the tibble from 1 all the way up to nlag.
#'
#' This function requires that the first column in your tibble is called
#' "date" and represents some kind of time epoch. Note also that you will
#' get no NA's returned. This means that the returned data will start nlag-1
#' days later than the input data.
#'
#' @importFrom tibble tibble
#' @importFrom stats embed setNames
#' @param x the data tibble to operate on
#' @param nlag the max number of lags to create for each variable
#'
#' @return a new tibble with the data lagged
#' @export
#'
#' @examples
#' library(dplyr)
#' tmpdf <- tibble(
#'   date = seq(from = as.Date("2018-01-01"), length.out = 10, by = "1 day"),
#'   a = 1:10, b = 11:20, d = 21:30
#' )
#' lagdf <- tmpdf %>% lagvariables(4)
#' lagdf
#' tmpdf
lagvariables <- function(x, nlag = 5) {
  stopifnot("date" %in% colnames(x))
  tmpdf <- x
  tibblify <- function(x) {
    data.frame(x) %>%
      tibble::rownames_to_column("date") %>%
      dplyr::as_tibble()
  }
  namify <- function(x) lapply(names(x), function(name) stats::setNames(x[[name]], c("date", paste0(name, "_", names(x[[name]])[-1]))))
  embedAndRename <- function(x) {
    a <- stats::embed(x, nlag)
    colnames(a) <- paste0("l", 0:(nlag - 1))
    rownames(a) <- as.character(tmpdf$date[nlag:nrow(tmpdf)])
    a %>% tibblify()
  }
  tmplist <- dplyr::select(tmpdf, -date) %>%
    lapply(embedAndRename) %>%
    namify()
  Reduce(function(x, y) dplyr::inner_join(x, y, by = "date"), tmplist)
}
