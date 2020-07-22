#' Convert an iso date to simple isoweek format
#'
#' Simplistic representation of an isoweek format is encoded here. Basically we convert 2017-11-23 to 201747 thus YYYY-MM-DD to YYYYWW.
#'
#' @param dates ISOdates as string vector
#'
#' @return a vector of strings with ISOweeks inside
#' @export
#' @examples
#' dateToIsoWeek(c("2017-11-23", "2015-11-23"))
dateToIsoWeek <- function(dates) gsub("-W|-\\d", "", ISOweek::date2ISOweek(dates))

#' Generate public holidays for all markets
#'
#' The function heavily relies on the timeDate package in R for all holidays.
#' Specifically it utilizes the listHolidays function to figure out which holidays
#' are existing.
#'
#' @param years the years for which to generate the holidays given as 2013:2017
#' as an example where the default is the last three years
#' @param country specifies which specific country to focus on except the "global"
#' holidays where the possible values are "CA", "CH", "DE", "FR", "GB", "IT", "JP", "US"
#'
#' @return a one hot encoded tibble where each column is a holiday and the first
#' column is the ISO date
#' @export
#'
#' @examples
#' b <- genHolidays(2016:2017)
#' b[1:10, 1:5]
genHolidays <- function(years = (lubridate::year(Sys.Date()) - 2):lubridate::year(Sys.Date()), country) {
  # a<-sapply(timeDate::listHolidays(), FUN=function(x) as.character(as.Date(eval(call(x, years)))))
  a <- sapply(timeDate::listHolidays(), FUN = function(x) as.character(as.Date(eval(as.call(list(utils::getFromNamespace(x, "timeDate"), years))))))
  tmpdf <- tibble(Date = as.character(seq(
    from = as.Date(paste0(min(years), "-01-01")),
    to = as.Date(paste0(max(years), "-12-31")),
    by = "1 day"
  )))
  toInd <- function(x) {
    dplyr::left_join(tmpdf, tibble(Date = x, Val = 1))
  }

  b <- lapply(seq_along(a), FUN = function(x) toInd(a[[x]]) %>% stats::setNames(c("Date", names(a)[x])))
  b <- Reduce(dplyr::inner_join, b)
  b <- dplyr::mutate_all(b, naToZero)
  countries <- c("CA", "CH", "DE", "FR", "GB", "IT", "JP", "US")
  if (missing(country)) {
    b <- dplyr::select(b, -dplyr::matches(paste0(countries, collapse = "|")))
  } else {
    b <- dplyr::select(b, -dplyr::matches(paste0(setdiff(countries, country), collapse = "|")))
  }
  b
}
