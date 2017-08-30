library(plotly)

# Read in data
df <-
  read.csv(
    "https://cdn.rawgit.com/plotly/datasets/master/GanttChart-updated.csv",
    stringsAsFactors = F
  )

# Convert to dates
df$Start <- as.Date(df$Start, format = "%m/%d/%Y")

# Sample client name
client = "Sample Client"

#' Plot a Gantt chart using plotly
#'
#' A Gantt chart is plotted using plotly. The format of the data.frame given to
#' the function should be
#'   Task      Start Duration Resource
#' Task 1   1/1/2016       50        A
#' Task 2  2/20/2016       25        B
#' Task 3   1/1/2016      100        C
#' Task 4  4/10/2016       60        C
#' Task 5   6/9/2016       30        C
#' Task 6  4/10/2016      150        A
#' Task 7   9/7/2016       80        B
#' Task 8 11/26/2016       10        B
#' where the duration is given in days.
#'
#' @param mydf the data.frame containing the information of the project
#' @param client the client name given as a string
#'
#' @return a plotly object
#' @export
#'
#' @examples
#' tmpdf <- structure(list(Task = c("Task 1", "Task 2", "Task 3", "Task 4",
#'          "Task 5", "Task 6", "Task 7", "Task 8"), Start = c("1/1/2016", "2/20/2016",
#'          "1/1/2016", "4/10/2016", "6/9/2016", "4/10/2016", "9/7/2016", "11/26/2016"),
#'          Duration = c(50L, 25L, 100L, 60L, 30L, 150L, 80L, 10L),
#'          Resource = c("A", "B", "C", "C", "C", "A", "B", "B")),
#'          .Names = c("Task", "Start", "Duration", "Resource"), class = "data.frame",
#'          row.names = c(NA, -8L))
#' plotGantt(tmpdf, "Sample Client")
plotGantt <- function(mydf, client) {
  # Choose colors based on number of resources
  cols <- RColorBrewer::brewer.pal(length(unique(mydf$Resource)), name = "Set3")
  mydf$color <- factor(mydf$Resource, labels = cols)

  # Initialize empty plot
  p <- plot_ly()

  # Each task is a separate trace
  # Each trace is essentially a thick line plot
  # x-axis ticks are dates and handled automatically

  for (i in 1:(nrow(mydf) - 1)) {
    p <- add_trace(p, x = c(mydf$Start[i], mydf$Start[i] + mydf$Duration[i]),
                   y = c(i, i), mode = "lines",
                   line = list(color = mydf$color[i], width = 20), showlegend = F,
                   hoverinfo = "text",

                   text = paste("Task: ", mydf$Task[i],"\n",
                                "Duration: ", mydf$Duration[i], "days\n",
                                "Resource: ", mydf$Resource[i]),
                   evaluate = T  # needed to avoid lazy loading
    )
  }

  p <- layout(p,
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
