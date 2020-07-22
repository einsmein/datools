
context("Dummy")
expect_equal(1, 1)

context("Gantt")
tmpdf <- structure(list(
  Task = c("Task 1", "Task 2", "Task 3", "Task 4", "Task 5", "Task 6", "Task 7", "Task 8"),
  Start = structure(c(16801, 16851, 16801, 16901, 16961, 16901, 17051, 17131), class = "Date"),
  Duration = c(50L, 25L, 100L, 60L, 30L, 150L, 80L, 10L),
  Resource = c("A", "B", "C", "C", "C", "A", "B", "B")
),
.Names = c("Task", "Start", "Duration", "Resource"),
row.names = c(NA, -8L), class = "data.frame"
)
a <- plotGantt(tmpdf, "Sample Client")
expect_is(a, "plotly")

context("Value functions")
tmpvec <- c(NA, 3:5, NA, NA, 4, 4, 10, NA)
expect_equal(sum(naToZero(tmpvec)), sum(tmpvec, na.rm = T))
expect_equal(sum(naToZero(tmpvec)), 30)
expect_equal(sum(naToZero(tmpvec)), sum(naToVal(tmpvec, 0)))

expect_equal(sum(nonFinToVal(tmpvec)), sum(tmpvec, na.rm = T))
expect_equal(sum(nonFinToVal(tmpvec)), 30)
expect_equal(sum(nonFinToVal(tmpvec, 0)), sum(naToVal(tmpvec, 0)))

context("Date functions")
expect_equal(c("201747", "201548", "200953"), datools::dateToIsoWeek(c("2017-11-23", "2015-11-23", "2009-12-31")))

context("Data encoding")
# res <- structure(list(
#   Data = structure(17167:17176, class = "Date"),
#   Sun = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L),
#   Mon = c(0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
#   Tues = c(0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
#   Wed = c(0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L),
#   Thurs = c(0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L),
#   Fri = c(0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L),
#   Sat = c(0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L)),
#   class = "data.frame", .Names = c("Data", "Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"),
#   row.names = c(NA,-10L))
# expect_equal(res, oneHotEncoder(x=seq(as.Date("2017-01-01"), by ="day", length.out = 10), f=function(x) lubridate::wday(x, label = TRUE)))

context("Time series")
x <- sin(seq(1, 10, length.out = 30))
y <- lag(x, 5) + rnorm(length(x), 0, 0.2)
expect_gte(abs(find_lag(x, y)), 4)
expect_lte(abs(find_lag(x, y)), 6)

context("Range operations")
expect_equal(
  rangeToBuckets(1:nrow(mtcars), 9),
  list(1:9, 10:18, 19:27, 28:32)
)
expect_equal(rangeToBuckets(1:3, 5), list(1:3))
expect_equal(rangeToBuckets(1:3, 3), list(1:3))
expect_equal(rangeToBuckets(1:3, 2), list(1:2, 3L))
expect_equal(rangeToBuckets(-2:3, 4), list(-2:1, 2:3))
