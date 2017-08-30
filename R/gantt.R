#' Plot a Gantt chart using plotly
#'
#' A Gantt chart is plotted using plotly. The format of the data.frame given to
#' the function should be
#'   Task      Start Duration Resource
#' Task 1 2016-01-01       50        A
#' Task 2 2016-02-20       25        B
#' Task 3 2016-01-01      100        C
#' Task 4 2016-04-10       60        C
#' Task 5 2016-06-09       30        C
#' Task 6 2016-04-10      150        A
#' Task 7 2016-09-07       80        B
#' Task 8 2016-11-26       10        B
#' where the duration is given in days.
#'
#' Originally source appeared on \url{https://www.r-bloggers.com/gantt-charts-in-r-using-plotly/}.
#'
#' @param mydf the data.frame containing the information of the project
#' @param client the client name given as a string
#' @param brewerpalname the name of the palette to use from RColorBrewer which defaults to "Set3"
#' @importFrom plotly add_trace plot_ly layout
#' @importFrom RColorBrewer brewer.pal
#'
#' @return a plotly object
#' @export
#'
#' @examples
#' tmpdf <- structure(list(Task = c("Task 1", "Task 2", "Task 3", "Task 4",
#' "Task 5", "Task 6", "Task 7", "Task 8"), Start = structure(c(16801,
#' 16851, 16801, 16901, 16961, 16901, 17051, 17131), class = "Date"),
#' Duration = c(50L, 25L, 100L, 60L, 30L, 150L, 80L, 10L), Resource = c("A",
#' "B", "C", "C", "C", "A", "B", "B")), .Names = c("Task", "Start",
#' "Duration", "Resource"), row.names = c(NA, -8L), class = "data.frame")
#' plotGantt(tmpdf, "Sample Client")
plotGantt <- function(mydf, client, brewerpalname = "Set3") {
  # Choose colors based on number of resources
  cols <- RColorBrewer::brewer.pal(length(unique(mydf$Resource)), name = brewerpalname)
  mydf$color <- factor(mydf$Resource, labels = cols)

  # Initialize empty plot
  p <- plotly::plot_ly()

  # Each task is a separate trace
  # Each trace is essentially a thick line plot
  # x-axis ticks are dates and handled automatically

  for (i in 1:(nrow(mydf) - 1)) {
    p <- plotly::add_trace(p, x = c(mydf$Start[i], mydf$Start[i] + mydf$Duration[i]),
                   y = c(i, i), mode = "lines",
                   line = list(color = mydf$color[i], width = 20), showlegend = F,
                   hoverinfo = "text",

                   text = paste("Task: ", mydf$Task[i],"\n",
                                "Duration: ", mydf$Duration[i], "days\n",
                                "Resource: ", mydf$Resource[i]),
                   evaluate = T  # needed to avoid lazy loading
    )
  }

  p <- plotly::layout(p,
                      xaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6")),
                      yaxis = list(showgrid = F, tickfont = list(color = "#e6e6e6"),
                                   tickmode = "array", tickvals = 1:nrow(mydf),
                                   ticktext = unique(mydf$Task), domain = c(0, 0.9)),

                      # Annotations
                      annotations = list(
                        # Add total duration and total resources used
                        # x and y coordinates are based on a domain of [0,1] and not
                        # actual x-axis and y-axis values

                        list(xref = "paper", yref = "paper", x = 0.80, y = 0.1,
                             text = paste0("Total Duration: ", sum(mydf$Duration), " days\n",
                                           "Total Resources: ", length(unique(mydf$Resource)), "\n\n"),
                             font = list(color = "#ffff66", size = 12),
                             ax = 0, ay = 0, align = "left"),

                        # Add client name and title on top
                        list(xref = "paper", yref = "paper", x = 0.1, y = 1, xanchor = "left",
                             text = paste0("Gantt Chart: ", client),
                             font = list(color = "#f2f2f2", size = 20, family = "Times New Roman"),
                             ax = 0, ay = 0, align = "left")),

                      plot_bgcolor = "#333333",
                      # Chart area color
                      paper_bgcolor = "#333333"
  )  # Axis area color
  p
}
