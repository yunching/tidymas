source("scripts/pmad_toolkit.R")
blpConnect()

opt <- c("CDR"="5D")
start_date <- "20200123"
end_date <- "20200724"

# Define assets in Black-Litterman model
bl_list <- c("SPX Index", "MXWOU Index", "RTY Index", "MXEF Index", "LEGATRUU Index")

# Define asset return calculation type
bl_returns <- tribble(
  ~BBG_Ticker, ~Return_type,
  "SPX Index", "Price",
  "MXWOU Index", "Price",
  "RTY Index", "Price",
  "MXEF Index", "Price",
  "LEGATRUU Index", "Price",
)

# Fetch historical data
bl_data <- fetch_bbg_data(bl_list, start_date, end_date, opt )

# Calculate historical returns
bl_returns <- bl_data %>%
  group_by(BBG_Ticker) %>%
  add_returns(bl_returns)

# Estimate covariance matrix
bl_cov <- bl_returns %>%
  est_cov_matrix("lw")
bl_cov

# Recover std dev
bl_std_dev <- bl_cov %>% diag() %>% sqrt()
bl_std_dev

# Recover correlation matrix
(diag(bl_std_dev) %*% t(diag(bl_std_dev)))

bl_cor <-  bl_cov / (diag(bl_std_dev) %*% t(diag(bl_std_dev)))
bl_cor

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


rf <- 0.05
spx <- 0.12
tmp_cov %*% tmp_wts * (spx - rf) /(tmp_cov %*% tmp_wts)[1, 1] + rf



