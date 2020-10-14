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
    mutate(period_return = asset_return(Close, lag(Close, 1), calc_type=Return_type))

  trades_scaled <- trades_w_returns %>%
    group_by(BBG_Ticker) %>%
    summarise(mean = mean(period_return, na.rm = TRUE), sd = sd(period_return, na.rm = TRUE), .groups = "keep")

  trades_final <- trades_w_returns %>%
    na.omit() %>%
    left_join(trades_scaled) %>%
    mutate(scaled_ret = period_return / sd,
           winsorised_ret = case_when(
             scaled_ret > 2 ~ 2,
             scaled_ret < -2 ~ -2,
             scaled_ret <= 2 ~ scaled_ret
           )
    )
    # Do not arrange in generic functions - leave it to user to decide manually
    # arrange(BBG_Ticker, date)

  return(trades_final)
}

# Estimates covariance matrix using RiskPortfolios package
est_cov_matrix <- function(returns_df, type = "large"){
  returns_df %>%
    select(date, BBG_Ticker, period_return) %>%
    # Scale it to percent
    # mutate(period_return = period_return * 100) %>%
    pivot_wider(names_from = BBG_Ticker, values_from = period_return) %>%
    column_to_rownames(var = "date") %>%
    as.matrix() %>%
    covEstimation(control = list(type = type))
}

get_risk_from_cov <- function(covar_mat){
  covar_mat %>%
    diag() %>%
    sqrt
}

get_cor_from_cov <- function(covar_mat){
  std_dev <- covar_mat %>% get_risk_from_cov()
  covar_mat / (std_dev %*% t(std_dev))
}
