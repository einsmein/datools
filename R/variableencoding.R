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
#' oneHotEncoder(x=seq(as.Date("2017-01-01"), by ="day", length.out = 10),
#'               f=function(x) wday(x, label = TRUE))
#' oneHotEncoder(x=wday(seq(as.Date("2017-10-07"), by ="days", length.out = 10), label = TRUE))
oneHotEncoder<-function(x, f = function(y) y){
  stopifnot(all(length(x)>1, is.function(f)))
  Data<-NULL
  tibble(Data=seq_along(x), Value=f(x)) %>%
    reshape2::dcast(Data~Value, value.var = "Value", fun.aggregate=length) %>%
    dplyr::mutate(Data=x) #%>% dplyr::select(Data, everything())
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
#' oneHotEncoder2(x=seq(as.Date("2017-01-01"), by ="day", length.out = 10),
#'               f=function(x) wday(x, label = TRUE))
#' oneHotEncoder2(x=wday(seq(as.Date("2017-10-07"), by ="days", length.out = 10), label = TRUE))
oneHotEncoder2 <- function(x, f = function(y) y){
  stopifnot(all(length(x)>1, is.function(f)))
  Category<-Data<-Value<-NULL
  tibble(Category=f(x), Data = seq_along(x), Value=1) %>%
    tidyr::spread(Category, Value, fill = 0) %>% mutate(Data=x)
}
