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
    mutate(period_return = asset_return(Close, lag(Close, 1), calc_type = Return_type))

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

scale_and_winsorise <- function(trades_w_returns) {
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

est_cov_matrix <- function(returns_df, type = "large", time_scale = 1){
  returns_df %>%
    select(date, BBG_Ticker, period_return) %>%
    # Scale it to percent
    # mutate(period_return = period_return * 100) %>%
    pivot_wider(names_from = BBG_Ticker, values_from = period_return) %>%
    column_to_rownames(var = "date") %>%
    as.matrix() %>%
    covEstimation(control = list(type = type)) * time_scale
}

get_risk_from_cov <- function(covar_mat){
  covar_mat %>%
    diag() %>%
    sqrt

  # Use %>% as.data.frame() %>% rownames_to_column() for tidier output
}

get_cor_from_cov <- function(covar_mat){
  std_dev <- covar_mat %>% get_risk_from_cov()
  covar_mat / (std_dev %*% t(std_dev))
}

# Risk contribution calculations
portfolio_risk <- function(sec_wts, covar){
  sqrt(sec_wts %*% covar %*% sec_wts)[1,1]
}

mcar <- function(sec_wts, covar){
  ans <- (covar %*% sec_wts) / portfolio_risk(sec_wts, covar)
  return(ans[, 1])
}

tcar <- function(sec_wts, covar){
  ans <- ((covar %*% sec_wts) / portfolio_risk(sec_wts, covar)) * sec_wts
  return(ans[, 1])
}

pct_tcar <- function(sec_wts, covar){
  ans <- (((covar %*% sec_wts) / portfolio_risk(sec_wts, covar)) * sec_wts) / portfolio_risk(sec_wts, covar)
  return(ans[, 1])
}

# Check it matches spreadsheet
# tmpcovar <- matrix(data = c( 0.001369,	0.001184,	0.000161,	0.001924,	0.002886,
#                              0.001184,	0.006400,	0.000580,	0.008840,	0.006240,
#                              0.000161,	0.000580,	0.000841,	0.001885,	0.001131,
#                              0.001924,	0.008840,	0.001885,	0.016900,	0.003380,
#                              0.002886,	0.006240,	0.001131,	0.003380,	0.067600),
#                    nrow = 5
# )
#
# tmp_sec_wts <- c(0.042,	0.25,	0.32,	0.22,	0.168)
#
# portfolio_risk(tmp_sec_wts, tmpcovar)
# mcar(tmp_sec_wts, tmpcovar)
# tcar(tmp_sec_wts, tmpcovar)
# pct_tcar(tmp_sec_wts, tmpcovar)
