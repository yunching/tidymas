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


# Optimal weights
rfr <- 0.004
rets <- c(0.009569556,	0.010465018,	0.007725633,	0.013568234,	0.010499082,
          0.009657741,	0.011729587,	0.008408488,	0.00767536,	0.007320136)

rets2 <- c(0.010969556,	0.010827413,	0.008005549,	0.014063864,	0.010660512,
           0.010048074,	0.012278413,	0.008626124,	0.007791434,	0.007483799)

covar <- matrix(data=c(0.0118,	0.0031,	0.0024,	0.0042,	0.0014,	0.0033,	0.0046,	0.0018,	0.0010,	0.0014,
                       0.0031,	0.0072,	0.0019,	0.0043,	0.0022,	0.0033,	0.0046,	0.0020,	0.0002,	0.0018,
                       0.0024,	0.0019,	0.0040,	0.0031,	0.0001,	0.0024,	0.0043,	0.0021,	0.0012,	0.0016,
                       0.0042,	0.0043,	0.0031,	0.0119,	0.0026,	0.0049,	0.0061,	0.0033,	0.0020,	0.0022,
                       0.0014,	0.0022,	0.0001,	0.0026,	0.0077,	0.0016,	0.0018,	0.0009,	0.0007,	0.0008,
                       0.0033,	0.0033,	0.0024,	0.0049,	0.0016,	0.0042,	0.0038,	0.0019,	0.0011,	0.0014,
                       0.0046,	0.0046,	0.0043,	0.0061,	0.0018,	0.0038,	0.0093,	0.0041,	0.0018,	0.0024,
                       0.0018,	0.0020,	0.0021,	0.0033,	0.0009,	0.0019,	0.0041,	0.0038,	0.0017,	0.0019,
                       0.0010,	0.0002,	0.0012,	0.0020,	0.0007,	0.0011,	0.0018,	0.0017,	0.0066,	0.0005,
                       0.0014,	0.0018,	0.0016,	0.0022,	0.0008,	0.0014,	0.0024,	0.0019,	0.0005,	0.0031
                  ), nrow=10
                )
(solve(covar)  %*% (rets - rfr)) / sum(solve(covar)  %*% (rets - rfr))

optimal_weights <- function(rets, covar, rfr){
  (solve(covar)  %*% (rets - rfr)) / sum(solve(covar)  %*% (rets - rfr))
}

optimal_weights(rets, covar, rfr)
