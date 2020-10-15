source("scripts/pmad_toolkit.R")
blpConnect()

# opt <- c("CDR"="5D")
# Get 5 years of monthly data
opt <- c("periodicitySelection"="MONTHLY")
end_date <- Sys.Date()
start_date <- "-60CM"

# Black-Litterman market implied returns
# Assumes we normalise using SPX returns, which is in the top in covariance matrix and wts
bl_implied_returns <- function(covar_mat, wts, bm_pos="all", normalising_factor = TRUE, bm_return = 0.03, rf=0.01){
  if (normalising_factor == TRUE){
    if (is.numeric(bm_pos)){
      nf <-((bm_return - rf) /(covar_mat %*% wts)[bm_pos])
      impl_rets <- covar_mat %*% (wts * nf) + rf
    }
    else if (bm_pos == "all"){
      nf <- ((bm_return - rf) /(wts %*% covar_mat %*% wts))
      impl_rets <- covar_mat %*% (wts %*% nf) + rf
    }
    else {
      stop("Unknown bm_pos parameter")
    }
  }
  else {
    impl_rets <- covar_mat %*% (wts + rf)
  }

  return(impl_rets)
}

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

# Fetch and calculate market cap weights

bl_wts <- bdp(bl_list, "CUR_MKT_CAP") %>%
  rownames_to_column(var = "BBG_Ticker") %>%
  as_tibble() %>%
  # group_by(BBG_Ticker) %>%
  mutate(wts = CUR_MKT_CAP / sum(CUR_MKT_CAP))

bl_wts %>% pull(wts) %>% write.table("clipboard", sep="\t")

bl_data %>% pivot_wider(names_from = BBG_Ticker, values_from = Close) %>% view()
# Calculate historical returns
bl_returns <- bl_data %>%
  group_by(BBG_Ticker) %>%
  add_returns(bl_returns_type)

# Estimate covariance matrix
bl_cov <- bl_returns %>%
  est_cov_matrix("large", time_scale = 3)

bl_implied_returns(bl_cov, bl_wts %>% pull(wts), bm_return = 0.03, rf = 0.01, normalising_factor = FALSE) %>% round(4)

bl_cov <- bl_returns %>%
  est_cov_matrix("large", time_scale = 1)

bl_implied_returns(bl_cov, bl_wts %>% pull(wts), bm_return = 0.02, rf = 0.01)
expected_bm_ret <- 0.01
rfr <- 0.004

