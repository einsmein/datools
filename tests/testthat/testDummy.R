
context("Dummy")
expect_equal(1,1)

context("Gantt")
tmpdf <- structure(list(Task = c("Task 1", "Task 2", "Task 3", "Task 4", "Task 5", "Task 6", "Task 7", "Task 8"),
                        Start = structure(c(16801, 16851, 16801, 16901, 16961, 16901, 17051, 17131), class = "Date"),
                        Duration = c(50L, 25L, 100L, 60L, 30L, 150L, 80L, 10L),
                        Resource = c("A", "B", "C", "C", "C", "A", "B", "B")),
                   .Names = c("Task", "Start", "Duration", "Resource"),
                   row.names = c(NA, -8L), class = "data.frame")
a <- plotGantt(tmpdf, "Sample Client")
expect_is(a, "plotly")
