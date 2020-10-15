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
  # message(paste("Normalising factor:", (bm_return - rf) /(wts %*% covar_mat %*% wts)))


  # # if (is.numeric(bm_pos)) {
  # #   impl_rets <- covar_mat %*% wts %*% nf + rf
  # # }
  # # else if (bm_pos == "all"){
  # #   impl_rets <- covar_mat %*% wts %*% ((bm_return - rf) /(wts %*% covar_mat %*% wts)) + rf
  # #   # message(paste("Normalising factor:", (bm_return - rf) /(wts %*% covar_mat %*% wts)))
  # #   # (expected_bm_ret - rfr) / (t(bm_wts) %*% super_duper_mat %*% bm_wts)
  # # }
  # # else{
  # #   stop("Unknown bm_pos parameter")
  # # }
  #
  # return(impl_rets)
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

bl_implied_returns(bl_cov, bl_wts %>% pull(wts), bm_return = 0.03, rf = 0.01, bm_pos = 1)
# bl_implied_returns(bl_cov, bl_wts %>% pull(wts), anchor_asset = 0.04, rf = 0.01)
# bl_implied_returns(bl_cov, bl_wts %>% pull(wts), anchor_asset = 0.041, rf = 0.01)

bl_cov <- bl_returns %>%
  est_cov_matrix("large", time_scale = 1)

bl_implied_returns(bl_cov, bl_wts %>% pull(wts), anchor_asset = 0.02, rf = 0.01)
# bl_implied_returns(bl_cov, bl_wts %>% pull(wts), anchor_asset = 0.04, rf = 0.01)
# bl_implied_returns(bl_cov, bl_wts %>% pull(wts), anchor_asset = 0.041, rf = 0.01)

super_duper_mat <- matrix(data=c(
                          0.0118,	0.0031,	0.0024,	0.0042,	0.0014,	0.0033,	0.0046,	0.0018,	0.0010,	0.0014,
                          0.0031,	0.0072,	0.0019,	0.0043,	0.0022,	0.0033,	0.0046,	0.0020,	0.0002,	0.0018,
                          0.0024,	0.0019,	0.0040,	0.0031,	0.0001,	0.0024,	0.0043,	0.0021,	0.0012,	0.0016,
                          0.0042,	0.0043,	0.0031,	0.0119,	0.0026,	0.0049,	0.0061,	0.0033,	0.0020,	0.0022,
                          0.0014,	0.0022,	0.0001,	0.0026,	0.0077,	0.0016,	0.0018,	0.0009,	0.0007,	0.0008,
                          0.0033,	0.0033,	0.0024,	0.0049,	0.0016,	0.0042,	0.0038,	0.0019,	0.0011,	0.0014,
                          0.0046,	0.0046,	0.0043,	0.0061,	0.0018,	0.0038,	0.0093,	0.0041,	0.0018,	0.0024,
                          0.0018,	0.0020,	0.0021,	0.0033,	0.0009,	0.0019,	0.0041,	0.0038,	0.0017,	0.0019,
                          0.0010,	0.0002,	0.0012,	0.0020,	0.0007,	0.0011,	0.0018,	0.0017,	0.0066,	0.0005,
                          0.0014,	0.0018,	0.0016,	0.0022,	0.0008,	0.0014,	0.0024,	0.0019,	0.0005,	0.0031
                          ), nrow=10,
                          dimnames= list(c("GM",	"HD",	"IP",	"HPQ",	"MO",	"AXP",	"AA",	"DD",	"MRK",	"MMM"),
                                         c("GM",	"HD",	"IP",	"HPQ",	"MO",	"AXP",	"AA",	"DD",	"MRK",	"MMM"))
                  )
super_duper_mat

# bm_wts <- c(0.027133655394525,	0.119130434782609,	0.025636070853462,	0.142302737520129,	0.246908212560387,
#             0.105732689210950,	0.045346215780998,	0.061706924315620,	0.128035426731079,	0.098067632850242
# )
#
bm_Wts <- c(0.03,	0.12,	0.03,	0.14,	0.25,	0.11,	0.05,	0.06,	0.13,	0.10)

# bm_rets_nnf

super_duper_mat * bm_wts
super_duper_mat * t(bm_wts)
t(bm_wts) * super_duper_mat
bm_wts * super_duper_mat

rf <- 0.004
super_duper_mat %*% (bm_wts + rf)
super_duper_mat %*% t(bm_wts + rf)
t(bm_wts  + rf) %*% super_duper_mat
(bm_wts  + rf) %*% super_duper_mat


# normalising factor
(B2-B3)/MMULT(MMULT(B8:K8,B12:K21),TRANSPOSE(B8:K8))

expected_bm_ret <- 0.01
rfr <- 0.004

(expected_bm_ret - rfr) / (t(bm_wts) %*% super_duper_mat %*% bm_wts)
bl_implied_returns(super_duper_mat, wts=bm_wts, bm_return = expected_bm_ret, rf = rfr, normalising_factor =FALSE) %>% round(4)
bl_implied_returns(super_duper_mat, wts=bm_wts, bm_return = expected_bm_ret, rf = rfr, normalising_factor =TRUE) %>% round(4)
bl_implied_returns(super_duper_mat, wts=bm_wts, bm_return = expected_bm_ret, rf = rfr, normalising_factor =TRUE, bm_pos = 3) %>% round(4)
