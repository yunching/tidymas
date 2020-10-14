source("scripts/pmad_toolkit.R")
blpConnect()

# opt <- c("CDR"="5D")
# Get 5 years of monthly data
opt <- c("periodicitySelection"="MONTHLY")
end_date <- Sys.Date()
start_date <- "-60CM"

# Define assets in Black-Litterman model
bl_list <- c("SPX Index", "MXWOU Index", "RTY Index", "MXEF Index", "LEGATRUU Index")

# Define asset return calculation type
bl_returns_type <- tribble(
  ~BBG_Ticker, ~Return_type,
  "SPX Index", "Price",
  "MXWOU Index", "Price",
  "RTY Index", "Price",
  "MXEF Index", "Price",
  "LEGATRUU Index", "Price",
)

# Fetch historical data
bl_data <- fetch_bbg_data(bl_list, start_date, end_date, opt)

bl_data %>% pivot_wider(names_from = BBG_Ticker, values_from = Close) %>% view()
# Calculate historical returns
bl_returns <- bl_data %>%
  group_by(BBG_Ticker) %>%
  add_returns(bl_returns_type)

# Estimate covariance matrix
bl_returns %>%
  est_cov_matrix("naive") %>% get_cor_from_cov() %>% round(3)

bl_returns %>%
  est_cov_matrix("large") %>% get_cor_from_cov() %>% round(3)

bl_returns %>%
  est_cov_matrix("ewma") %>% get_cor_from_cov() %>% round(3)


# covar_mat %>% diag() %>% sqrt()
# covar_mat / (std_dev %*% t(bl_std_dev))

tmp_cov <- bl_returns %>%
  select(date, BBG_Ticker, period_return) %>%
  pivot_wider(names_from = BBG_Ticker, values_from = period_return) %>%
  column_to_rownames(var = "date") %>%
  cov()
tmp_cov

tmp_sd <- bl_returns %>%
  select(date, BBG_Ticker, period_return) %>%
  pivot_wider(names_from = BBG_Ticker, values_from = period_return) %>%
  column_to_rownames(var = "date") %>%
  cov() %>% diag() %>% sqrt()
tmp_sd

tmp_cov / (tmp_sd %*% t(tmp_sd))

tmp %*% t(tmp)
  get_cor_from_cov() %>%
  round(3)


# Recover std dev
bl_std_dev <- bl_cov %>%
  get_risk_from_cov()

# Recover correlation
bl_cor <- bl_cov %>%
  get_cor_from_cov()

tmp_cor <- matrix(data=c(1.0000,	0.8800,	0.8400,	0.8100,	-0.1600,
             0.8800,	1.0000,	0.8300,	0.8700,	0.0700,
             0.8400,	0.8300,	1.0000,	0.8300,	-0.1400,
             0.8100,	0.8700,	0.8300,	1.0000,	-0.0500,
             -0.1600,	0.0700,	-0.1400,	-0.0500,	1.0000), nrow = 5)
tmp_cor

tmp_wts <- c(0.24,	0.26,	0.03,	0.03,	0.44)
tmp_wts

tmp_sd <- c(0.149,	0.156,	0.192,	0.21,	0.058)
tmp_sd

tmp_cov <-tmp_sd %*% t(tmp_sd) * tmp_cor

# Check - recover sd
tmp_std_dev <- tmp_cov  %>% diag() %>% sqrt()

# Check - recover correlations
tmp_cor2 <-  tmp_cov / (tmp_std_dev %*% t(tmp_std_dev))

rf <- 0.05
spx <- 0.12
tmp_cov %*% tmp_wts * (spx - rf) /(tmp_cov %*% tmp_wts)[1, 1] + rf

# Assumes we normalise using SPX returns, which is in the top in covariance matrix and wts
bl_implied_returns <- function(covar_mat, wts, spx = 0.12, rf=0.05){
  tmp_cov %*% tmp_wts * (spx - rf) /(tmp_cov %*% tmp_wts)[1, 1] + rf
}

bl_implied_returns(tmp_cov, tmp_wts)

