# Load commonly used functions from PMAD toolkit
source("scripts/pmad_toolkit.R")

blpConnect()

# Global settings & definitions --------------------------------------------

opt <- c("CDR"="5D")
start_date <- "20200123"
end_date <- "20200724"

# TODO add inflation return type?
asset_return <- function(current_obs, prev_obs, calc_type) {
  ret <- NA

  if (calc_type[1] == "Price") {
    ret <- (current_obs - prev_obs) / prev_obs
  } else if (calc_type[1] == "Yield"){
    ret <- (prev_obs - current_obs)
  } else {
    warning("Unknown calc type")
  }

  ret
}

# Helper function for linear regression
add_factors_data <- function(df){
  inner_join(df, factors_ts, by="date")
}

# Helper function for linear regression
mod_fun <- function(df){
  lm(winsorised_ret ~ . -date, data = df)
}

# Trade calculations ------------------------------------------------------

# Process trades
trades_ticker_list <- c("SPX Index", "UKX Index", "USYC1030 Index", "AUDEUR Curncy", "AUDJPY Curncy", "NZDEUR Curncy",
                        "NZDJPY Curncy", "GTESP10Y Govt", "GTFRF10Y Govt", "GTBEF10Y Govt",
                        "CNYUSD Curncy", "EURUSD Curncy", "GTCNY10YR Corp", "USYC5Y30 Index",
                        "USGG10YR Index", "UKYC2Y10 Index", "GTDEM10Y Govt", "DEYC5Y30 Index",
                        "GTITL5Y Govt", "GTESP5Y Govt")

# lookup table controlling return calculations
trades_return_type <- tribble(
  ~BBG_Ticker, ~Return_type,
  "USYC1030 Index", "Yield",
  "CNYUSD Curncy", "Price",
  "AUDNZDvEURJPY", "Price",
  "SPX Index", "Price",
  "EURUSD Curncy", "Price",
  "10Y_SP_vs_FR_BE", "Yield",
  "GTCNY10YR Corp", "Yield",
  "USYC5Y30 Index", "Yield",
  "USGG10YR Index", "Yield",
  "AUDEUR Curncy", "Price",
  "NZDJPY Curncy", "Price",
  "AUDJPY Curncy", "Price",
  "NZDEUR Curncy", "Price",
  "UK_2S10_flattener", "Yield",
  "10Y_FR_vs_DE", "Yield",
  "DE_5S30_flattener", "Yield",
  "5Y_IT_vs_ES", "Yield"
)

trades_data <- fetch_bbg_data(trades_ticker_list, start_date, end_date, opt)

# check data downloaded (by alphabetical order)
trades_data$BBG_Ticker %>% unique() %>% sort() %>% as_tibble()

# transform trades which are composite tickers
trades_data_transformed <- trades_data %>%
  select(BBG_Ticker, date, Close) %>%
  pivot_wider(names_from = BBG_Ticker, values_from = Close) %>%
  transmute(date,
            `USYC1030 Index`,
            `CNYUSD Curncy`,
            `AUDEUR Curncy`,
            `NZDJPY Curncy`,
            `AUDJPY Curncy`, `NZDEUR Curncy`,
            AUDNZDvEURJPY = 0.5 * `AUDEUR Curncy` + 0.5 * `NZDJPY Curncy`,
            `5Y_IT_vs_ES` = `GTITL5Y Govt` -  `GTESP5Y Govt`,
            `SPX Index`,
            "UK_2S10_flattener" = -`UKYC2Y10 Index`,
            "10Y_FR_vs_DE" = `GTFRF10Y Govt` - `GTDEM10Y Govt`,
            "DE_5S30_flattener" = -`DEYC5Y30 Index`,
            `EURUSD Curncy`,
            `10Y_SP_vs_FR_BE` = 0.5 * (`GTESP10Y Govt` - `GTFRF10Y Govt`) + 0.5 * (`GTESP10Y Govt` - `GTBEF10Y Govt`),
            `GTCNY10YR Corp`,
            `USYC5Y30 Index`,
            `USGG10YR Index`
  ) %>%
  pivot_longer(-date, names_to = "BBG_Ticker", values_to = "Close") %>%
  group_by(BBG_Ticker)

trades_final <- add_returns(trades_data_transformed, trades_return_type)
trades_final
# trades_final %>%  filter(BBG_Ticker == "SPX Index") %>% write_csv("tmp.csv")


trades_cov <- trades_final %>%
  est_cov_matrix()


# Factor model calculations -----------------------------------------------
factor_ticker_list <- c("TACUSAU Index", #FTSE US All Cap TR
                          "ASX Index",   #FTSE UK All Share
                          "FTR4EXUK Index", #FTSE Europe ex UK
                          "WIJPN Index", #FTSE Japan
                          "GTII10 Govt", #US 10Y real rates
                          "GTGBPII10Y Govt", #UK 10Y real rates
                          "RR10HDE Index", #German 10Y real rates
                          "RR10HIT Index", #Italy 10Y real rates
                          "GTJPYII10Y Govt", #Japan 10Y real rates
                          "GTAUDII10Y Govt", #Australia 10Y real rates
                          "GTCADII10Y Govt", #Canada 10Y real rates
                          "USGGBE10 Index", #US 10Y inflation
                          "UKGGBE10 Index", #UK 10Y inflation
                          "DEGGBE10 Index", #German 10Y inflation
                          "JYGGBE10 Index", #Japan 10Y inflation
                          "ADGGBE10 Index", #Australia 10Y inflation
                          "CDGGBE10 Index", #Canada 10Y inflation
                          "BCOMEN Index", #Bloomberg Energy subindex
                          "BCOMXE Index", #Bloomberg Commodities ex energy subindex
                          "EURUSD Curncy",
                          "JPYUSD Curncy"
)

factors_data <- fetch_bbg_data(factor_ticker_list, start_date, end_date, opt)

factor_return_type <- tribble(
  ~BBG_Ticker, ~Return_type,
  "econ_growth_us", "Price",
  "econ_growth_uk", "Price",
  "econ_growth_eu", "Price",
  "econ_growth_jp", "Price",
  "real_rates_us", "Yield",
  "real_rates_uk", "Yield",
  "real_rates_de", "Yield",
  "real_rates_spread_it" , "Yield",
  "real_rates_jp", "Yield",
  "real_rates_au", "Yield",
  "real_rates_ca", "Yield",
  "cpi_us", "Yield",
  "cpi_uk", "Yield",
  "cpi_de", "Yield",
  "cpi_jp", "Yield",
  "cpi_au", "Yield",
  "cpi_ca", "Yield",
  "commod_oil", "Price",
  "commod_ex_oil", "Price",
  "fx_eur", "Price",
  "fx_jpy", "Price"
)

factors_transformed <- factors_data %>%
  pivot_wider(names_from = BBG_Ticker, values_from = Close) %>%
  transmute(
    date = date,
    econ_growth_us = `TACUSAU Index`,
    econ_growth_uk =  `ASX Index`,
    econ_growth_eu = `FTR4EXUK Index`,
    econ_growth_jp = `WIJPN Index`,
    real_rates_us = `GTII10 Govt`,
    real_rates_uk = `GTGBPII10Y Govt`,
    real_rates_de = `RR10HDE Index`,
    real_rates_spread_it = `RR10HIT Index` - real_rates_de,
    real_rates_jp = `GTJPYII10Y Govt`,
    real_rates_au = `GTAUDII10Y Govt`,
    real_rates_ca = `GTCADII10Y Govt`,
    cpi_us = `USGGBE10 Index`,
    cpi_uk = `UKGGBE10 Index`,
    cpi_de = `DEGGBE10 Index`,
    cpi_jp = `JYGGBE10 Index`,
    cpi_au = `ADGGBE10 Index`,
    cpi_ca = `CDGGBE10 Index`,
    commod_oil = `BCOMEN Index`,
    commod_ex_oil = `BCOMXE Index`,
    fx_eur = `EURUSD Curncy`,
    fx_jpy = `JPYUSD Curncy`
  ) %>%
  pivot_longer(-date, names_to = "BBG_Ticker", values_to = "Close") %>%
  # left_join(factor_return_type, by="BBG_Ticker") %>%
  group_by(BBG_Ticker)

factors_final <- add_returns(factors_transformed, factor_return_type)
factors_final

factors_cov <- factors_final %>%
  est_cov_matrix()

# Model estimation --------------------------------------------------------
trades_ts <- trades_final %>%
  pivot_wider(date, names_from = "BBG_Ticker", values_from = "winsorised_ret")

factors_ts <- factors_final %>%
  pivot_wider(date, names_from = "BBG_Ticker", values_from = "winsorised_ret")

full_data <- trades_ts %>%
  inner_join(factors_ts)

regressions <- trades_final %>%
  select(BBG_Ticker, date, winsorised_ret) %>%
  group_by(BBG_Ticker) %>%
  nest() %>%
  #take each trade data and combine with factor data
  mutate(regression_data = map(data, add_factors_data)) %>%
  #run multivariate regression on each trade against same factors
  mutate(model = map(regression_data, mod_fun),
         tidied_model = map(model, tidy),
         top_results = map(model, glance)
  )

r_squared <- regressions %>%
  select(`BBG_Ticker`, top_results) %>%
  unnest(top_results) %>%
  select(`BBG_Ticker`, r.squared, adj.r.squared) %>%
  pivot_longer(-BBG_Ticker, names_to = 'term', values_to = "value") %>%
  pivot_wider(names_from = BBG_Ticker, values_from = value)

factor_estimates <- regressions %>%
  select(`BBG_Ticker`, tidied_model) %>%
  unnest(tidied_model) %>%
  select(BBG_Ticker, term, estimate) %>%
  pivot_wider(names_from = "BBG_Ticker", values_from = "estimate")

results <- factor_estimates %>%
  bind_rows(r_squared)

write_csv(results, "factor_exposure_w_rsquared.csv")
return(results)



# Correlation analysis ----------------------------------------------------

# Assumptions
# - Based on past 6 month's daily returns
# - For FI spread trades, assumes leg with smaller duration is scaled up to ma

hist_cor_trades <- trades_final %>%
  select(date, BBG_Ticker, winsorised_ret) %>%
  filter(!BBG_Ticker %in% c("AUDEUR Curncy", "NZDJPY Curncy", "AUDJPY Curncy",
                            "NZDEUR Curncy")) %>%
  pivot_wider(names_from = BBG_Ticker, values_from = winsorised_ret) %>%
  select(-date) %>%
  cor() %>%
  as_tibble()

hist_cor_trades
write_csv(hist_cor_trades, "hist_corr_trades.csv")

hist_cor_factors <- factors_final %>%
  select(date, BBG_Ticker, winsorised_ret) %>%
  pivot_wider(names_from = BBG_Ticker, values_from = winsorised_ret) %>%
  select(-date) %>%
  cor() %>%
  as_tibble()

hist_cor_factors
write_csv(hist_cor_factors, "hist_corr_factors.csv")

# Testing -----------------------------------------------------------------

tmp <- trades_final %>%
  # ungroup() %>%
  select(date, BBG_Ticker, daily_return) %>%
  filter(!BBG_Ticker %in% c("AUDEUR Curncy", "NZDJPY Curncy", "AUDJPY Curncy",
                            "NZDEUR Curncy")) %>%
  pivot_wider(names_from = BBG_Ticker, values_from = daily_return)

# trades_final %>%
#   filter(BBG_Ticker == "EURUSD Curncy")
#
# factors_final %>%
#   filter(factors == "fx_eur") %>%

tmp <- trades_final %>%
  # ungroup() %>%
  select(date, BBG_Ticker, daily_return) %>%
  filter(!BBG_Ticker %in% c("AUDEUR Curncy", "NZDJPY Curncy", "AUDJPY Curncy",
                            "NZDEUR Curncy")) %>%
  pivot_wider(names_from = BBG_Ticker, values_from = daily_return)

cor.test(tmp$`USYC1030 Index`, tmp$`CNYUSD Curncy`) %>% tidy()
cor.test(tmp$`USYC1030 Index`, tmp$`USYC5Y30 Index`)

#calculating volatility of trades
# TODO document how the conversion factors are obtained
trade_size_conversion_factor <- read_csv("./scripts/trade_size_conversion_factor.csv")

trades_semiannual_SD <- trades_scaled %>%
  left_join(trade_size_conversion_factor, by="BBG_Ticker") %>%
  mutate(semiannual_SD = sd*size*conversion*sqrt(nrow(tmp)-1))

install.packages("FRAPO")
library(FRAPO)

write_csv(trades_cov, "trades_cov.csv")
weights <- read_csv("./scripts/weights.csv") %>%
  column_to_rownames(var = "Trade") %>%
  as.matrix()

marginal_contribution <- mrc(weights, trades_cov, percentage = TRUE) %>%
  as.matrix()

crossprod(marginal_contribution,weights)

trades_returns_date <- trades_final %>%
  select(1,5)

write_csv(trades_returns_date, "trial.csv")

returns <- read_csv("./scripts/test volatility.csv")
sd(returns)

# Calculate mahalanobis distance
# Mahalanobis distance
# distance squared
shock <- c(20, 20, 20)
d_sq <- shock %*% covar[1:3, 1:3] %*% shock
vars <- length(shock)
1-pchisq(d_sq, df=vars)
