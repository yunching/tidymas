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
bar.max_drawdown <- bar.processed %>%
  summarise(max_drawdown = min(drawdown))

#Calculate max drawdown period
bar.max_drawdown_period <- bar.processed %>%
  select(drawdown_day) %>%
  (function(x) {max(rle(x$drawdown_day)$lengths)})

