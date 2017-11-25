#' Convert an iso date to simple isoweek format
#'
#' Simplistic representation of an isoweek format is encoded here. Basically we convert 2017-11-23 to 201747 thus YYYY-MM-DD to YYYYWW.
#'
#' @param dates ISOdates as string vector
#'
#' @return a vector of strings with ISOweeks inside
#' @export
#' @importFrom ISOweek date2ISOweek
#' @examples
#' dateToIsoWeek(c("2017-11-23", "2015-11-23"))
dateToIsoWeek <- function (dates) gsub("-W|-\\d", "", ISOweek::date2ISOweek(dates))
