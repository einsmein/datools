#' Make waterfall data tibble from a tibble of cash flows
#'
#' The only requirement for this function to work is that the first row and last row of the tibble
#' represent the cash in and cash out. In other words the Base and the Total when talking about
#' sales modeling.
#'
#' @param data the data tibble to operate on
#'
#' @return a new data tibble with columns necessary to create a waterfall visualization
#' @importFrom dplyr lag mutate transmute
#' @export
#'
#' @examples
#' datadf <-
#'   tibble::tibble(
#'     desc = c(
#'       "starting_net_income",
#'       "write_offs",
#'       "sales",
#'       "COGS",
#'       "overhead",
#'       "taxes",
#'       "bonuses",
#'       "other_income",
#'       "ending_net_income"
#'     ),
#'     amount = c(1200, -2300, 11000, -9000, -1900, -1200, -3000, 7000, 1800)
#'   )
#' to_waterfall(datadf)
#' # Change order
#' datadf2 <- to_waterfall(datadf)
#' firstindex <- which(datadf$desc == "sales")
#' datadf2[c(firstindex, setdiff(seq_along(datadf$amount), firstindex)), ]
to_waterfall <- function(data) {
  desc <- amount <- end <- id <- type <- start <- NULL
  datadf <- data %>%
    mutate(
      desc = factor(desc, levels = desc),
      # desc = as.factor(desc),
      id = seq_along(amount),
      type = ifelse(amount > 0, "in", "out"),
      end = cumsum(amount),
      start = dplyr::lag(end)
    )
  datadf[c(1, nrow(datadf)), "type"] <- "net"
  datadf[1, "start"] <- 0
  datadf[nrow(datadf), "end"] <- 0
  datadf <-
    datadf %>% dplyr::transmute(id, desc, type = as.factor(type), start, end, amount)
  datadf
}

#' Make a waterfall plot from a tibble of cash flows
#'
#' The only requirement for this function to work is that the first row and last row of the tibble
#' represent the cash in and cash out. In other words the Base and the Total when talking about
#' sales modeling.
#'
#' @param data the data tibble to operate on
#' @param label adding labels to plot. If true, data must contain `label` column
#' @param nudge_scale scale to nudge the labels away from the bar
#' @param ... other parameters passed to `geom_text`
#' @importFrom ggplot2 aes element_text geom_rect theme theme_minimal xlab ylab scale_fill_brewer
#'
#' @return a ggplot of the waterfall plot
#' @export
#'
#' @examples
#' library(ggplot2)
#' datadf <-
#'   tibble::tibble(
#'     desc = c(
#'       "starting_net_income",
#'       "write_offs",
#'       "sales",
#'       "COGS",
#'       "overhead",
#'       "taxes",
#'       "bonuses",
#'       "other_income",
#'       "ending_net_income"
#'     ),
#'     amount = c(1200, -2300, 11000, -9000, -1900, -1200, -3000, 7000, 1800)
#'   )
#' plot_waterfall(datadf) +
#'   scale_fill_brewer(type = "seq", palette = 4) +
#'   theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust = 1))
plot_waterfall <- function(data, label=TRUE, nudge_scale=1, ...) {
  desc <- amount <- end <- id <- type <- start <- NULL
  pl <- to_waterfall(data) %>% ggplot(aes(desc, fill = type)) +
    geom_rect(aes(
      x = desc,
      xmin = id - 0.45,
      xmax = id + 0.45,
      ymin = end,
      ymax = start
    )) +
    theme_minimal() +
    xlab("") +
    ylab("Value") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1))
  if(label) {
    pl <- pl +
      geom_text(aes(label=data$label,
                    y=end + ifelse(end >= start, 1, -1) * nudge_scale * max(end-start)/20),
      ...)
  }
  pl
}
#
