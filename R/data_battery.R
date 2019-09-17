#' Load BBG Data
#'
#' @return Loads sqlite data with data
#' @export
#'
#' @examples load_bbg_data()
load_bbg_data <- function(){
  # create a db of tickers
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "mydatabase.db")
  # summary(con)
  # DBI::dbListTables(con)

  # select ticker list from db
  sec_db <- DBI::dbReadTable(con, 'sec_db')

  sec_db %>%
    select(BBG_Ticker) -> sec_list

  # str(sec_list)
  message(sec_list)

  # download BBG data
  Rblpapi::blpConnect()
  raw_data <- Rblpapi::bdh(sec_list$BBG_Ticker, c("PX_OPEN", "PX_HIGH", "PX_LOW","PX_LAST", "VOLUME"),
                           start.date=Sys.Date()-10 * 252)

  # collapse into 1 table
  raw_data %>%
    bind_rows(.id = "Ticker") %>%
    as_tibble() ->
    bbg_data

  # na.omit causes issues for securities that don't have all columns available e.g. GTDEM10Y Govt

  # write data to db
  DBI::dbWriteTable(con, "px_db", bbg_data, overwrite = TRUE)

  # check data from px_db
  DBI::dbReadTable(con, 'px_db')
  # use lubridate's as_date function to convert integer back to date


  # disconnect from db
  DBI::dbDisconnect(con)
  message("Data extract completed.")
}

#' Load BBG ticker to list for data load
#'
#' @param bbg_ticker BBG Ticker to include
#'
#' @return Include BBG Ticker to table for data load
#' @export
#'
#' @examples \donttest{add_bbg_ticker("AAPL US Equity")}
add_bbg_ticker <- function(bbg_ticker){
  # create a db of tickers
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "mydatabase.db")

  query <- paste("INSERT INTO sec_db (BBG_Ticker) VALUES ('", bbg_ticker, "');", sep="")
  # message(query)

  rs <- DBI::dbSendStatement(con, query)
  # message(DBI::dbHasCompleted(rs))
  # message(DBI::dbGetRowsAffected(rs))
  DBI::dbDisconnect(con)
}

#' Remove BBG Ticker from data load
#'
#' @param bbg_ticker BBG Ticker to remove from data load
#'
#' @return Exclude BBG Ticker to table for data load
#' @export
#'
#' @examples \donttest{rm_bbg_ticker("AAPL US Equity")}
rm_bbg_ticker <- function(bbg_ticker){
  # create a db of tickers
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "mydatabase.db")

  query <- paste("DELETE FROM sec_db WHERE BBG_Ticker = '", bbg_ticker, "';", sep="")
  # message(query)

  rs <- DBI::dbSendStatement(con, query)
  # message(DBI::dbHasCompleted(rs))
  # message(DBI::dbGetRowsAffected(rs))

  DBI::dbDisconnect(con)
}
