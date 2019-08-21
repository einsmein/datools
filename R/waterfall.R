#' Make waterfall data tibble from a tibble of cash flows
#'
#'
#' @param data the data tibble to operate on
#'
#' @return a new data tibble with columns necessary to create a waterfall visualization
#' @importFrom stats lag
#' @export
#'
#' @examples
#' datadf <-
#' tibble::tibble(
#'   desc = c(
#'     'starting_net_income',
#'     'write_offs',
#'     'sales',
#'     'COGS',
#'     'overhead',
#'     'taxes',
#'     'bonuses',
#'     'other_income',
#'     'ending_net_income'
#'   ),
#'   amount = c(1200, -2300, 11000, -9000, -1900, -1200, -3000, 7000, 1800)
#' )
#' to_waterfall(datadf)
to_waterfall <- function (data) {
  desc<-amoun<-end<-id<-type<-start<-NULL
  datadf <- data %>%
    mutate(
      desc = factor(desc, levels=desc),
      id = seq_along(amount),
      type = ifelse(amount > 0, 'in', 'out'),
      end = cumsum(amount),
      start = stats::lag(end)
    )
  datadf[c(1, nrow(datadf)), "type"] <- 'net'
  datadf[1, "start"] <- 0
  datadf[nrow(datadf), "end"] <- 0
  datadf <-
    datadf %>% dplyr::transmute(id, desc, type = as.factor(type), start, end, amount)
  datadf
}

#' Make a waterfall plot from a tibble of cash flows
#'
#' @param data the data tibble to operate on
#'
#' @return a ggplot of the waterfall plot
#' @export
#'
#' @examples
#' datadf <-
#' tibble::tibble(
#'   desc = c(
#'     'starting_net_income',
#'     'write_offs',
#'     'sales',
#'     'COGS',
#'     'overhead',
#'     'taxes',
#'     'bonuses',
#'     'other_income',
#'     'ending_net_income'
#'   ),
#'   amount = c(1200, -2300, 11000, -9000, -1900, -1200, -3000, 7000, 1800)
#' )
#' plot_waterfall(datadf)
plot_waterfall <- function(data) {
  to_waterfall(data) %>% ggplot(aes(desc, fill = type)) +
    geom_rect(aes(
      x = desc,
      xmin = id - 0.45,
      xmax = id + 0.45,
      ymin = end,
      ymax = start
    )) + theme_minimal() +
    xlab("") + ylab("Value") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))
}
