<<<<<<< HEAD
=======
library(tidyverse)
library(readxl)
bar <- read_excel("Z:\\Downloads\\example3_4.xls")

#' Augment a time series with new features
#'
#' @description Augments a time series data frame with quantitative calculations
#' @param data Dataframe containing data
#' @param date Column containing date information. Defaults to "date".
#' @param price Column containing price information. Defailts to "PX_LAST".
#' @param rf_rate Risk-free rate for calculating excess returns
#'
#' @return A dataframe containing extra features.
#' @export
#'
#' @examples
ts_augment <- function(data, date = date, price = PX_LAST, rf_rate = 0.04 / 252){
  require(dplyr)
  date <- rlang::enquo(date)
  price <- rlang::enquo(price)

  data %>%
    as_tibble() %>%
    select(date = !!date,
           price = !!price) %>%
    mutate(dailyret = (price - lag(price, 1)) / lag(price, 1),
           excess_dailyret = dailyret - rf_rate) %>%
    tidyr::drop_na() %>%
    mutate(cumret = cumprod(1 + dailyret) - 1,
           high_watermark = cummax(cumret),
           drawdown = (1 + cumret) / (1 + high_watermark) - 1,
           drawdown_day = if_else(drawdown != 0, 1, 0)
    )
}

ts_summary <- function(data){
  max_drawdown <- data %>%
    summarise(max_drawdown = min(drawdown))

  list(max_drawdown)
}

#Calculate max drawdown
# bar.max_drawdown <- bar.processed %>%
#   summarise(max_drawdown = min(drawdown))
#
# #Calculate max drawdown period
# bar.max_drawdown_period <- bar.processed %>%
#   select(drawdown_day) %>%
#   (function(x) {max(rle(x$drawdown_day)$lengths)})

#' Cache Bloomberg data locally
#'
#' @param ticker Single ticker to save OHLC and volume data
#' @param start.date Start date
#' @param end.date End date
#'
#' @return Saves a csv file in the data2 folder
#' @export
#'
#' @examples
get_bbg_data <- function(ticker, start.date = Sys.Date() - 365 * 1, end.date = Sys.Date()){
  filename <- stringr::str_replace_all(ticker, " ", "_")
  filename <- paste0(start.date, "_", end.date, "_", filename)

  #Download all data in single call so it runs fast
  data <- Rblpapi::bdh(ticker,
               fields = c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST", "VOLUME"),
               start.date = start.date,
               end.date = end.date) %>%
          dplyr::bind_rows(.id = "Ticker") %>%
          tibble::as_tibble()

  #But save each ticker into its own csv file
  data %>%
    readr::write_csv(file.path(getwd(), "data2", paste0(filename, ".csv")))
}

get_many_bbg_data <- function(ticker, start.date = Sys.Date() - 365 * 1, end.date = Sys.Date()){
  # filename <- stringr::str_replace_all(ticker, " ", "_")
  filename_header <- paste0(start.date, "_", end.date, "_")

  #Download all data in single call so it runs fast
  data <- Rblpapi::bdh(ticker,
                       fields = c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST", "VOLUME"),
                       start.date = start.date,
                       end.date = end.date)

  #But save each ticker into its own csv file

  purrr::walk2(data, ticker,
             function(edata, eticker){
              eticker <- stringr::str_replace_all(eticker, " ", "_")
              readr::write_csv(edata, file.path(getwd(), "data2", paste0(filename_header, eticker, ".csv")))
             })
}

# TBC
# stationary_screen <- function(data){
#
# }

bar.max_drawdown <- bar.processed %>%
  summarise(max_drawdown = min(drawdown))

#Calculate max drawdown period
bar.max_drawdown_period <- bar.processed %>%
  select(drawdown_day) %>%
  (function(x) {max(rle(x$drawdown_day)$lengths)})

