library(tidyverse)
library(grid)
library(gridExtra)
library(Rblpapi)
#library(MacrobondAPI)
library(xts)
library(lubridate)
library(forecast)
library(ggfortify)

#Establish connection with Bloomberg
opt <- c("CDR"="5D")
blpConnect()

#' Get data from Bloomberg using the Rblpapi package, with sensible defaults
#'
#' @param ticker A character string containing a Bloomberg Ticker to download
#' @param start_date Start date to retrieve data. Defaults to one year ago.
#' @param end_date End date ti retrieve data. Defaults to today.
#' @param fields A list of Bloomberg fields to download. Defaults to PX_LAST
#'
#' @return A data frame containing the fields of provided ticker for a given period of time.
#' @export
#'
#' @examples
#' getData("AAPL US Equity")
getData <- function(ticker, start_date = Sys.Date()-365, end_date = today(), fields = c("PX_LAST")){
  dailymon_db %>%
    filter(BBG_Ticker %in% ticker) %>%
    filter(start_date >= start_date) %>%
    filter(end_date <=end_date)

  #bbg_data <- bdh(ticker, fields, start.date = start_date, end.date = end_date)
  #colnames(bbg_data) <- c("Date", "Close")
  #as_tibble(bbg_data)
}

#' Get price time series from Bloomberg, using the Rblpapi package
#'
#' @inheritParams getData
#' @return A xts time series containing OHLC data
#' @export
#'
#' @examples
#' getData_xts("AAPL US Equity")
#' getData_xts("GBPUSD Curncy")
getData_xts <- function(ticker, start_date = Sys.Date()-365, end_date = today()){
  bbg_data <- bdh(ticker, c("PX_OPEN", "PX_HIGH","PX_LOW","PX_LAST"), start.date = start_date, end.date = end_date)
  my_ts <- xts(cbind(bbg_data$PX_OPEN, bbg_data$PX_HIGH, bbg_data$PX_LOW, bbg_data$PX_LAST), order.by = bbg_data$date)
  colnames(my_ts) <- c("Open", "High", "Low", "Close")
  my_ts
}

fetch_daily_bbg_data <- function(trades_ticker_list, start_date, end_date, opt) {
  bdh(trades_ticker_list, c("PX_OPEN", "PX_HIGH","PX_LOW","PX_LAST", "VOLUME"), start_date, end_date, options = opt) %>%
    bind_rows(.id = "BBG_Ticker" ) %>%
    as_tibble
}

#' Draws a simple time series plot
#'
#' @param ticker Character string containing Bloomberg ticker
#' @param title Character string to use in chart title
#' @param yield_mode Boolean flag to indicate if subtitle should indicate 1-day yield changes instead of 1-day percentage returns
#' @param start_date Start date for chart
#'
#' @export
#'
#' @examples
#' ggTS("AAPL US Equity")
ggTS <- function(ticker, title = ticker, yield_mode = FALSE, start_date = Sys.Date()-365){
  # data <- read_csv("./BBG_snapshot.csv", col_types = cols()) %>%
  #   filter(Ticker == !!ticker)
  data <- getData(ticker = {{ ticker }}, start_date = start_date)
  if (yield_mode == FALSE){
    my_subtitle <- paste("Last:", format(round(data$PX_LAST[length(data$PX_LAST)], digits = 2), big.mark = ","),
                         "1D ret:", paste0(format(round((data$PX_LAST[length(data$PX_LAST)] / data$PX_LAST[length(data$PX_LAST) - 1] - 1) * 100, 2), nsmall = 2), "%"),
                         "L:", format(round(min(data$PX_LAST), digits = 2), big.mark = ","),
                         "H:", format(round(max(data$PX_LAST), digits = 2), big.mark = ",")
    )
    ggplot2::ggplot(data, ggplot2::aes(x = date, y = PX_LAST)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(colour = "blue", size = 0.8) +
      ggplot2::labs(title = title, y = "Price", x = "Date", subtitle = my_subtitle)
  }
  else {
    my_subtitle <- paste("Last:", data$PX_LAST[length(data$PX_LAST)],
                         "1D yld chg:", paste0(format(round(data$PX_LAST[length(data$PX_LAST)] - data$PX_LAST[length(data$PX_LAST)-1] , 2), nsmall = 3), "%"),
                         "L:", format(round(min(data$PX_LAST), digits = 2), big.mark = ","),
                         "H:", format(round(max(data$PX_LAST), digits = 2), big.mark = ",")
    )
    ggplot2::ggplot(data, ggplot2::aes(x = date, y = PX_LAST)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(colour = "blue", size = 0.8) +
      ggplot2::labs(title = title, y = "Yield", x = "Date", subtitle = my_subtitle)
  }


}

getMB <- function(mb_ticker){
  my_data <- FetchOneTimeSeries(mb_ticker)
  if (getIsError(my_data))
    stop(getErrorMessage(my_data))

  #as.xts(my_data)
  xts(getValues(my_data), order.by = getDatesAtEndOfPeriod(my_data))
}

#Get BBG data (for econs series). See getData() to get BBG market data.
getBBG <- function(ticker, start_date = Sys.Date()-365* 10){
  bbg_data <- bdh(ticker, c("PX_LAST"), start.date = start_date)
  bbg_data <- xts(bbg_data[,-1], order.by = bbg_data[, 1])
  colnames(bbg_data) <- ticker
  bbg_data
}

ggXTS <- function(my_xts, title = "Value", series_subset = ""){
  if (length(colnames(my_xts)) > 1){
    stop("Multiple data series detected in plot.")
  }

  subtitle <- paste("Last:", round(my_xts[length(my_xts), 1], digits = 2),
                    paste0("(",format(index(my_xts)[length(my_xts)], "%b-%y"), ")"),
                    "Prev:", round(my_xts[length(my_xts) - 1, 1], digits = 2),
                    "Chg:", format(round((coredata(my_xts[length(my_xts), 1]) - coredata(my_xts[length(my_xts) - 1, 1])), digits = 2), nsmall = 2)
  )

  #, ts.colour = 'dodgerblue3'
  #autoplot(my_xts) + labs(title = title, subtitle = subtitle, y = "Index", x = "Date")
  ggplot(my_xts, aes(x = Index, y = my_xts[,1])) + geom_line(col = "dodgerblue3")  + labs(title = title, subtitle = subtitle, y = "Index", x = "Date")
}

m_yoy <- function(series){
  result <- (series / lag(series, 12) - 1) * 100
  na.omit(result)
}

m_mom <- function(series){
  result <- (series / lag(series, 1) - 1) * 100
  na.omit(result)
}

q_qoq <- function(series){
  result <- (series / lag(series, 1) - 1) * 100
  na.omit(result)
}

q_yoy <- function(series){
  result <- (series / lag(series, 4) - 1) * 100
  na.omit(result)
}

y_yoy <- function(series){
  result <- (series / lag(series, 1) - 1) * 100
  na.omit(result)
}

hline <- function(y_val){
  geom_hline(yintercept = y_val, color = "black", linetype = "dashed")
}

#' Returns a blank chart to add grid layouts
#'
#' @return A blank plot
#' @export
#'
blank_chart <- function(){
  grid.rect(gp=gpar(col="white"), draw = FALSE)
}

sec_summary <- function(my_df){
  Chg <- my_df %>% mutate(Chg = Close - lag(Close, 1)) %>% select(Chg)
  Chg %>% summary() %>% print()
  cat("\n")
  paste("Standard deviation: ", round(sd(Chg$Chg, na.rm = TRUE), digits = 2)) %>%  cat()
  Chg %>% na.omit() %>% ggplot(aes(Chg)) + geom_bar(stat = "bin", bins = 30)
}

bulk_load_data <- function(){
  require(dplyr)
  sec_list <- c("RX1 Comdty",
  "G 1 Comdty",
  "IK1 Comdty",
  "OAT1 Comdty",
  "GBPUSD Curncy",
  "GBPEUR Curncy",
  "UKTWBROA Index",
  "GTGBP2Y Govt",
  "GTGBP5Y Govt",
  "GTGBP10Y Govt",
  "GTGBP30Y Govt",
  "UKGGBE05Y Index",
  "UKGGBE10Y Index",
  "UKGGBE30Y Index",
  "GB0BPR Index",
  "EURUSD Curncy",
  "EURGBP Curncy",
  "GTDEM2Y Govt",
  "GTDEM5Y Govt",
  "GTDEM10Y Govt",
  "GTDEM30Y Govt",
  "GTESP2Y Govt",
  "GTESP5Y Govt",
  "GTESP10Y Govt",
  "GTESP30Y Govt",
  "GTITL2Y Govt",
  "GTITL5Y Govt",
  "GTITL10Y Govt",
  "GTITL30Y Govt",
  "EURCHF Curncy",
  "SNBN SW Equity",
  "BBDXY Index",
  "GT2 Govt",
  "GT5 Govt",
  "GT10 Govt",
  "GT30 Govt",
  "USGGBE02 Index",
  "USGGBE05 Index",
  "USGGBE10 Index",
  "USGGBE30 Index",
  "SPX Index",
  "INDU Index",
  "CCMP Index",
  "RTY Index",
  "UKX Index",
  "MCX Index",
  "ASX Index",
  "SX5E Index",
  "DAX Index",
  "CAC Index",
  "SMI Index",
  "IBEX Index",
  "FTSEMIB Index",
  "BVLX Index",
  "ASE Index",
  "NKY Index",
  "HSI Index",
  "KOSPI Index",
  "STI Index",
  "VIX Index",
  "V2X Index",
  "V1X Index",
  "VFTSE Index",
  "CL1 Comdty",
  "CO1 Comdty",
  "XAU Curncy",
  "XAG Curncy",
  "C 1 Comdty",
  "W 1 Comdty",
  "FB US Equity",
  "AAPL US Equity",
  "AMZN US Equity",
  "NFLX US Equity",
  "GOOG US Equity",
  "TSLA US Equity",
  "SPOT US Equity",
  "MSFT US Equity",
  "V US Equity",
  "ORCL US Equity",
  "MA US Equity",
  "CRM US Equity",
  "IBM US Equity",
  "CSCO US Equity",
  "INTC US Equity",
  "NVDA US Equity",
  "AMD US Equity",
  "MU US Equity",
  "TXN US Equity",
  "AMAT US Equity",
  "6954 JT Equity",
  "6861 JT Equity",
  "JPM US Equity",
  "WFC US Equity",
  "BAC US Equity",
  "C US Equity",
  "GS US Equity",
  "MS US Equity",
  "BLK US Equity",
  "BK US Equity",
  "STT US Equity",
  "NTRS US Equity",
  "BX US Equity",
  "KKR US Equity",
  "APO US Equity",
  "CG US Equity",
  "OAK US Equity",
  "III LN Equity",
  "XBTUSD BGN Curncy",
  "XRPUSD BGN Curncy",
  "XETUSD BGN Curncy",
  "XLCUSD BGN Curncy",
  "5 HK Equity",
  "144 HK Equity",
  "939 HK Equity",
  "941 HK Equity",
  "1398 HK Equity",
  "3988 HK Equity",
  "1548 HK Equity",
  "BABA US Equity",
  "BIDU US Equity",
  "TCEHY US Equity",
  "BZUN US Equity",
  "CAPL SP Equity",
  "GENS SP Equity"
  )
  Rblpapi::bdh(sec_list, "PX_LAST", Sys.Date()-365) %>%
    dplyr::bind_rows(.id = "Ticker") %>%
    tibble::as_tibble() %>%
    write_csv("./scripts/BBG_snapshot.csv")
}
