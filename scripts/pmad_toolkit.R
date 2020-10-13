# Common functions used by PMAD

library(tidyverse)
library(broom)
library(lubridate)
library(RiskPortfolios)
library(Rblpapi)


# Pull and tidy data from Bloomberg
fetch_bbg_data <- function(trades_ticker_list, start_date, end_date, opt) {
  bdh(trades_ticker_list, "PX_LAST", start_date, end_date, options = opt) %>%
    bind_rows(.id = "BBG_Ticker" ) %>%
    as_tibble() %>%
    rename(Close = PX_LAST)
}

# Calculate returns
add_returns <- function(trades_data_transformed, trades_return_type) {
  trades_w_returns <- trades_data_transformed %>%
    left_join(trades_return_type, by="BBG_Ticker") %>%
    group_by(`BBG_Ticker`) %>%
    mutate(daily_return = asset_return(Close, lag(Close, 1), calc_type=Return_type))

  trades_scaled <- trades_w_returns %>%
    group_by(BBG_Ticker) %>%
    summarise(mean = mean(daily_return, na.rm = TRUE), sd = sd(daily_return, na.rm = TRUE), .groups = "keep")

  trades_final <- trades_w_returns %>%
    na.omit() %>%
    left_join(trades_scaled) %>%
    mutate(scaled_ret = daily_return / sd,
           winsorised_ret = case_when(
             scaled_ret > 2 ~ 2,
             scaled_ret < -2 ~ -2,
             scaled_ret <= 2 ~ scaled_ret
           )
    ) %>%
    arrange(BBG_Ticker, date)

  return(trades_final)
}
