#' Save a single CSV-table into a single table sqlite database
#'
#' This function is useful for taking huge CSV files that do not fit into RAM
#' and store them as a SQLite db which can be streamed via dplyr. This way you
#' can work with really big data files without being limited by your machines RAM.
#'
#' @param csv_file name of the CSV file to convert
#' @param sqlite_file name of the newly created sqlite file
#' @param table_name name of the table to store the data table in the sqlite dbase
#' @param pre_process_size the number of lines to check the data types of the individual columns (default 1000)
#' @param chunk_size the number of lines to read for each chunk (default 50000)
#' @param delim the field delimiter to use (default ,)
#' @importFrom RSQLite SQLite dbWriteTable
#' @importFrom readr read_delim spec
#' @importFrom DBI dbConnect
#' @importFrom dplyr select_if %>% src_sqlite
#' @importFrom lubridate is.Date is.POSIXt
#' @examples
#' library(RSQLite)
#' library(dplyr)
#' sqlite_file <- "example.sqlite"
#' table_name <- "example"
#' write.csv(airquality, "example.csv", row.names=FALSE)
#' csvToSQLite("example.csv", sqlite_file, table_name,
#'               pre_process_size = 1000, chunk_size = 50000)
#' mydb <- src_sqlite(sqlite_file, create = FALSE)
#' mydata <- tbl(mydb, table_name)
#' head(mydata)
#' @export
csvToSQLite <- function(csv_file, sqlite_file, table_name,
                        pre_process_size = 1000, chunk_size = 50000,
                        delim = ",") {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = sqlite_file)

  # read an extract of the data to extract the colnames and types
  # to figure out the date ande datetime columns
  df <- readr::read_delim(csv_file, delim, n_max = pre_process_size)
  date_cols <- df %>% dplyr::select_if(lubridate::is.Date) %>% colnames()
  datetime_cols <- df %>% dplyr::select_if(lubridate::is.POSIXt) %>% colnames()
  # write this first batch of lines to SQLITE table, converting dates to string representation
  df[ , date_cols] <- as.character.Date(df[ , date_cols])
  df[ , datetime_cols] <- as.character.POSIXt(df[ , datetime_cols])
  RSQLite::dbWriteTable(con, table_name, as.data.frame(df), overwrite = TRUE)

  # subfunction that appends new sections to the table
  append_to_sqlite <- function(x, pos) {
    x <- as.data.frame(x)
    x[ , date_cols] <- as.character.Date(x[ , date_cols])
    x[ , datetime_cols] <- as.character.POSIXt(x[ , datetime_cols])
    RSQLite::dbWriteTable(con, table_name, x, append = TRUE)
  }

  # readr chunk functionality
  readr::read_delim_chunked(csv_file, append_to_sqlite, delim = ",",
                            skip = pre_process_size, col_names = colnames(df),
                            col_types = readr::spec(df), chunk_size = chunk_size,
                            progress = FALSE)
  DBI::dbDisconnect(con)
}
