# Load commonly used functions from PMAD toolkit
source("scripts/pmad_toolkit.R")

blpConnect()

# Global settings & definitions --------------------------------------------

opt <- c("CDR"="5D")
# opt <- c("periodicitySelection"= "DAILY", "nonTradingDayFillMethod"="PREVIOUS_VALUE")
# We use 6 months' worth of daily data (6*20 = 120 data points)
start_date <- "20220630"
end_date <- "20221231"

# TODO add inflation return type?
# Yield returns are set up as yield changes, and later when multiplied by
# duration sizing would give first order returns
asset_return <- function(current_obs, prev_obs, calc_type) {
  ret <- NA

  if (calc_type[1] == "Price") {
    ret <- (current_obs - prev_obs) / prev_obs
  } else if (calc_type[1] == "Yield"){
    ret <- (current_obs - prev_obs)
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
# Step 1 - Add tickers required for analysis
trades_ticker_list <- c("USGGT02Y Index",
                        "USGGT05Y Index",
                        "USGGBE05 Index",
                        "USGG10YR Index",
                        "USGG30YR Index",
                        "USYC2Y5Y Index",
                        "USYC2Y10 Index",
                        "USYC5Y30 Index",
                        "BF020530 Index",
                        "DEYC2Y10 Index",
                        "GDBR10 Index",
                        "GJGB10 Index",
                        ".1Y2S10S Index",
                        "USDJPY Curncy",
                        "USDCNY Curncy",
                        "EURUSD Curncy",
                        "EURJPY Curncy",
                        "GBPUSD Curncy",
                        "GBPAUD Curncy",
                        "AUDJPY Curncy",
                        "AUDUSD Curncy",
                        "AUDNZD Curncy",
                        "EURGBP Curncy",
                        "EURAUD Curncy",
                        "AUDCAD Curncy",
                        "SPX Index",
                        "SX5E Index",
                        "XIN9I Index",
                        "MXCN Index",
                        "G0025 3M5Y BLC2 Curncy",
                        "G0025 3M30Y BLC2 Curncy",
                        "G0001 3M10Y BLC2 Curncy",
                        "G0001 3M2Y BLC2 Curncy"
) %>% unique()

trades_data <- fetch_bbg_data(trades_ticker_list, start_date, end_date, opt)

# check data downloaded (by alphabetical order)
trades_data$BBG_Ticker %>% unique() %>% sort() %>% as_tibble()

# lookup table controlling return calculations
# Step 2 - Indicate return type "Price" or "Yield"
trades_return_type <- tribble(
  ~BBG_Ticker, ~Return_type,
  "USGGT02Y Index", "Yield",
  "USGGT05Y Index", "Yield",
  "USGGBE05 Index", "Yield",
  "USGG10YR Index", "Yield",
  "USGG30YR Index", "Yield",
  "USYC2Y5Y Index", "Yield",
  "USYC2Y10 Index", "Yield",
  "USYC5Y30 Index", "Yield",
  "BF020530 Index", "Yield",
  "DEYC2Y10 Index", "Yield",
  "GJGB10 Index", "Yield",
  "GDBR10 Index", "Yield",
  ".1Y2S10S Index","Yield",
  "G0025 3M5Y BLC2 Curncy", "Yield",
  "G0025 3M30Y BLC2 Curncy", "Yield",
  "G0001 3M10Y BLC2 Curncy", "Yield",
  "G0001 3M2Y BLC2 Curncy", "Yield",
  "USDJPY Curncy", "Price",
  "USDCNY Curncy", "Price",
  "EURUSD Curncy", "Price",
  "EURAUD Curncy", "Price",
  "EURJPY Curncy", "Price",
  "GBPUSD Curncy", "Price",
  "GBPAUD Curncy", "Price",
  "AUDJPY Curncy","Price",
  "AUDUSD Curncy", "Price",
  "AUDNZD Curncy", "Price",
  "EURGBP Curncy", "Price",
  "AUDCAD Curncy", "Price",
  "SPX Index", "Price",
  "SX5E Index", "Price",
  "XIN9I Index", "Price",
  "MXCN Index", "Price"
)

trades_data_w_ret <- add_returns(trades_data, trades_return_type)

# Step 3 - Construct trades for Bloomberg tickers

# Multiply position size directly in transformation
# for fixed income, assume % return = yield curve chg in % * -duration contribution size in years
# which will only be correct to first order for long positions
# Bloomberg prints some yield tickers in % [typically for outright] and others in bps (%%) [typically for spreads]

# Flatteners/outright longs need to have a negative sign in front of the size,
# because it actually becomes unprofitable when yield increases!
# Don't need negative sign for steepeners and outright shorts

# when no sizes are provided, assume 1% R2 (FX + EQ) or 1 months (FI)
# NB: TYX might have changed the default sizing - to double check

trades_total <- trades_data_w_ret %>%
  select(BBG_Ticker, date, period_return) %>%
  pivot_wider(names_from = BBG_Ticker, values_from = period_return) %>%
  transmute(date,
            #IRSD trades
            #Short 5y real yield
            `IRSD_short_SPX_SX5E` = - 0.004 * `SPX Index` - 0.001 * `SX5E Index`,
            `IRSD_US_3M5S30_steepener` = 0.25/12 * 0.01 * (`G0025 3M30Y BLC2 Curncy` - `G0025 3M5Y BLC2 Curncy`),
            `IRSD_AU_3M2S10_steepener` = 0.25/12 * 0.01 * (`G0001 3M10Y BLC2 Curncy` - `G0001 3M2Y BLC2 Curncy`),
            `IRSD_short_EUR_against_USD_AUD` = 0.2 * 0.01 * -(`EURUSD Curncy` + `EURAUD Curncy`) * 0.5,
            `IRSD_short_GBP_against_USD_AUD` = 0.2 * 0.01 * -(`GBPUSD Curncy` + `GBPAUD Curncy`) * 0.5,
            `IRSD_long_China_EQ` = 0.2 * 0.01 * (`XIN9I Index` + `MXCN Index`) * 0.5,

            # `IRSD_short_us_5Y_real` = `USGGT05Y Index` * 0.5/12 * 0.01,
            # `IRSD_DE_2S10_flattener` = -`DEYC2Y10 Index` * 0.5/12 * 0.01 * 0.01,
            # `IRSD_US_1Y2S10S_steepener` = `.1Y2S10S Index` * 0.5/12 * 0.01,
            # `IRSD_long_dollar`    = 0.002 * (-`EURUSD Curncy` + `USDJPY Curncy` - `GBPUSD Curncy`),
            # `IRSD_long_USDCNY` = 0.0025 * `USDCNY Curncy`,

            #PMAD trades
            `PMAD_US_2S5S30_butterfly` = -`BF020530 Index` * 1/12 * 0.01 * 0.01,
            `PMAD_US_2S10_steepener` = `USYC2Y10 Index` * 1/12 * 0.01 * 0.01,
            `PMAD_long_bund_short_jgb` = (`GJGB10 Index` - `GDBR10 Index`) * 1/12 * 0.01,
            `PMAD_short_EUR_against_USD_JPY` = 0.5 * 0.01 * -(`EURUSD Curncy` + `EURJPY Curncy`) * 0.5,
            # `PMAD_long_US_30Y` = -`USGG30YR Index` * 1/12 * 0.01,
            # `PMAD_long_dollar`    = 0.001 * (-`EURUSD Curncy` + `USDJPY Curncy`),
            # `PMAD_short_AUDNZD`    = 0.001 * (-`AUDNZD Curncy`),
            `PMAD_long_SPX` = 0.5 * 0.01 * (`SPX Index`),

  )

write_csv(trades_total,"trades_total.csv")

total_returns <- trades_total %>% transmute(total = rowSums(across(where(is.numeric))))
total_returns_numeric <- as.numeric(unlist(total_returns))

total_annualised_vol <- sd(total_returns_numeric)*sqrt(252)*10000
message(paste("TE of all trades: ", total_annualised_vol))

write_csv(total_returns,"total_returns.csv")

trades_transformed <- trades_total %>%
  pivot_longer(-date, names_to = "BBG_Ticker", values_to = "period_return") %>%
  group_by(BBG_Ticker)

trades_final <- scale_and_winsorise(trades_transformed)

trades_final

write_csv(trades_final, "trades_final.csv")

p <- trades_final %>%
  # filter(period_return>0.3)
  # filter(BBG_Ticker == "USYC5Y30 Index") %>%
  ggplot(aes(x=date, y=period_return)) + geom_line(aes(color=BBG_Ticker))
ggplotly(p)

trades_cov <- trades_final %>%
  est_cov_matrix()

get_risk_from_cov(trades_cov) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  as_tibble() %>%
  rename(trade = "rowname", te=".")




#Results match against s/s

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

factors_final <- add_returns(factors_transformed, factor_return_type) %>%
  mutate(year=year(date), qtr=quarter(date))
#factors_final

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
  # mutate(year=year(date), qtr=quarter(date)) %>%
  # select(BBG_Ticker, date, year, qtr, winsorised_ret) %>%
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
  # filter(year ==2020, qtr == 3) %>%
  ungroup() %>%
  select(`BBG_Ticker`, top_results) %>%
  unnest(top_results) %>%
  select(`BBG_Ticker`, r.squared, adj.r.squared) %>%
  pivot_longer(-BBG_Ticker, names_to = 'term', values_to = "value") %>%
  pivot_wider(names_from = BBG_Ticker, values_from = value)

factor_estimates <- regressions %>%
  # filter(year ==2020, qtr == 3) %>%
  ungroup() %>%
  select(`BBG_Ticker`,tidied_model) %>%
  unnest(tidied_model) %>%
  select(BBG_Ticker, term, estimate) %>%
  pivot_wider(names_from = "BBG_Ticker", values_from = "estimate")


pct_tcar_fun <- function(df){
  # message(df[[1, 1]])
  # message(str(df))
  # message(df$estimate)
  ans <- pct_tcar(df$estimate, factors_cov)
  cbind(tribble(~factor,
                "econ_growth_us",
                "econ_growth_uk",
                "econ_growth_eu",
                "econ_growth_jp",
                "real_rates_us",
                "real_rates_uk",
                "real_rates_de",
                "real_rates_spread_it",
                "real_rates_jp",
                "real_rates_au",
                "real_rates_ca",
                "cpi_us",
                "cpi_uk",
                "cpi_de",
                "cpi_jp",
                "cpi_au",
                "cpi_ca",
                "commod_oil",
                "commod_ex_oil",
                "fx_eur",
                "fx_jpy"
  ), ans)

}

regressions %>%
  # filter(year ==2020, qtr == 3) %>%
  ungroup() %>%
  select(`BBG_Ticker`,tidied_model) %>%
  unnest(tidied_model) %>%
  filter(term!= "(Intercept)") %>%
  select(BBG_Ticker, term, estimate) %>%
  group_by(BBG_Ticker) %>%
  nest() %>%
  mutate(pct_tcar = map(data, pct_tcar_fun)) %>%
  select(BBG_Ticker, pct_tcar) %>%
  unnest(pct_tcar)

tmp <- regressions %>%
  # filter(year ==2020, qtr == 3) %>%
  ungroup() %>%
  select(`BBG_Ticker`,tidied_model) %>%
  unnest(tidied_model) %>%
  filter(term!= "(Intercept)") %>%
  select(BBG_Ticker, term, estimate) %>%
  group_by(BBG_Ticker) %>%
  nest()

results <- factor_estimates %>%
  bind_rows(r_squared) %>%
  bind_rows(get_risk_from_cov(trades_cov) * sqrt(252) * 10000)

write_csv(results, "factor_exposure_w_rsquared2.csv")

message("Factor model analysis completed.")
# #Systematic returns and risk---------------------------------------------
# intercepts <- factor_estimates %>%
#   filter(term=="(Intercept)") %>%
#   pivot_longer(cols = -term, names_to="trade", values_to="beta") %>%
#   select(trade, beta) %>%
#   left_join(trades_final, by = c("trade" = "BBG_Ticker")) %>%
#   mutate(term="(Intercept)", winsorised_ret = 1) %>% #Give winsorised_ret value of 1 to faciliate calculation of systematic ret further down
#   select(trade, date, term, beta, winsorised_ret) %>%
#   na.omit()
# intercepts
#
# systematic_ret <- factor_estimates %>%
#   pivot_longer(cols = "USYC1030 Index":"USGG10YR Index", names_to = "trade", values_to = "beta") %>%
#   left_join(factors_final, by = c("term" = "BBG_Ticker")) %>% #combine betas from factor_est with actual factor returns in factors_final
#   select(trade, date, term, beta, winsorised_ret) %>%
#   na.omit() %>% #clear na on joins as intercept terms do not have corresponding entries in factor_estimates
#   bind_rows(intercepts) %>%
#   arrange(trade, date, term) %>%
#   mutate(term_contrib = beta * winsorised_ret) %>%
#   group_by(trade, date) %>%
#   summarise(sys_ret = sum(term_contrib), .groups = "keep")
# systematic_ret %>% filter(sys_ret > 200)
#
# systematic_risk <- systematic_ret %>%
#   group_by(trade) %>%
#   summarise(sys_risk = sd(sys_ret))
#
# ## Haven't shown HW and ZY how to plot yet
# # Plot forecasted returns to check
# systematic_ret %>%
#   ggplot(aes(x=date, y = sys_ret)) + geom_line(aes(color=trade)) #CNY 10Y trade looks weird, should probably look better when sizes are included
#
#
# trades_final %>%
#   filter(BBG_Ticker == "GTCNY10YR Corp") %>%
#   ggplot(aes(x=date, y= winsorised_ret)) + geom_line()
#
# #Stress test----------------------------------------------------------------
# #Strong growth and inflation
# factors_pos_stress <- factors_final %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("econ_growth_us"),3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("econ_growth_uk"),3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("econ_growth_eu"),3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("econ_growth_jp"),3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_us"),3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_uk"),3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_de"),3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_jp"),3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_au"),3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_ca"),3*sd,winsorised_ret))
#
# pos_stress_ret <- factor_estimates %>%
#   pivot_longer(cols = "USYC1030 Index":"USGG10YR Index", names_to = "trade", values_to = "beta") %>%
#   left_join(factors_pos_stress, by = c("term" = "BBG_Ticker")) %>% #combine betas from factor_est with actual factor returns in factors_final
#   select(trade, date, term, beta, winsorised_ret) %>%
#   na.omit() %>% #clear na on joins as intercept terms do not have corresponding entries in factor_estimates
#   bind_rows(intercepts) %>%
#   arrange(trade, date, term) %>%
#   mutate(term_contrib = beta * winsorised_ret) %>%
#   group_by(trade, date) %>%
#   summarise(sys_ret = sum(term_contrib), .groups = "keep")
#
# #Poor growth and inflation
# factors_neg_stress <- factors_final %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("econ_growth_us"),-3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("econ_growth_uk"),-3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("econ_growth_eu"),-3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("econ_growth_jp"),-3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_us"),-3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_uk"),-3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_de"),-3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_jp"),-3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_au"),-3*sd,winsorised_ret)) %>%
#   mutate(winsorised_ret=ifelse(BBG_Ticker==c("cpi_ca"),-3*sd,winsorised_ret))
#
# neg_stress_ret <- factor_estimates %>%
#   pivot_longer(cols = "USYC1030 Index":"USGG10YR Index", names_to = "trade", values_to = "beta") %>%
#   left_join(factors_neg_stress, by = c("term" = "BBG_Ticker")) %>% #combine betas from factor_est with actual factor returns in factors_final
#   select(trade, date, term, beta, winsorised_ret) %>%
#   na.omit() %>% #clear na on joins as intercept terms do not have corresponding entries in factor_estimates
#   bind_rows(intercepts) %>%
#   arrange(trade, date, term) %>%
#   mutate(term_contrib = beta * winsorised_ret) %>%
#   group_by(trade, date) %>%
#   summarise(sys_ret = sum(term_contrib), .groups = "keep")
#
# # Correlation analysis ----------------------------------------------------
#
# # Assumptions
# # - Based on past 6 month's daily returns
# # - For FI spread trades, assumes leg with smaller duration is scaled up to ma
#
# hist_cor_trades <- trades_final %>%
#   select(date, BBG_Ticker, winsorised_ret) %>%
#   filter(!BBG_Ticker %in% c("AUDEUR Curncy", "NZDJPY Curncy", "AUDJPY Curncy",
#                             "NZDEUR Curncy")) %>%
#   pivot_wider(names_from = BBG_Ticker, values_from = winsorised_ret) %>%
#   select(-date) %>%
#   cor() %>%
#   as_tibble()
#
# hist_cor_trades
# write_csv(hist_cor_trades, "hist_corr_trades.csv")
#
# hist_cor_factors <- factors_final %>%
#   select(date, BBG_Ticker, winsorised_ret) %>%
#   pivot_wider(names_from = BBG_Ticker, values_from = winsorised_ret) %>%
#   select(-date) %>%
#   cor() %>%
#   as_tibble()
#
# hist_cor_factors
# write_csv(hist_cor_factors, "hist_corr_factors.csv")
#
# # Testing -----------------------------------------------------------------
#
# tmp <- trades_final %>%
#   # ungroup() %>%
#   select(date, BBG_Ticker, daily_return) %>%
#   filter(!BBG_Ticker %in% c("AUDEUR Curncy", "NZDJPY Curncy", "AUDJPY Curncy",
#                             "NZDEUR Curncy")) %>%
#   pivot_wider(names_from = BBG_Ticker, values_from = daily_return)
#
# # trades_final %>%
# #   filter(BBG_Ticker == "EURUSD Curncy")
# #
# # factors_final %>%
# #   filter(factors == "fx_eur") %>%
#
# tmp <- trades_final %>%
#   # ungroup() %>%
#   select(date, BBG_Ticker, daily_return) %>%
#   filter(!BBG_Ticker %in% c("AUDEUR Curncy", "NZDJPY Curncy", "AUDJPY Curncy",
#                             "NZDEUR Curncy")) %>%
#   pivot_wider(names_from = BBG_Ticker, values_from = daily_return)
#
# cor.test(tmp$`USYC1030 Index`, tmp$`CNYUSD Curncy`) %>% tidy()
# cor.test(tmp$`USYC1030 Index`, tmp$`USYC5Y30 Index`)
#
# #calculating volatility of trades
# # TODO document how the conversion factors are obtained
# trade_size_conversion_factor <- read_csv("./scripts/trade_size_conversion_factor.csv")
#
# trades_semiannual_SD <- trades_scaled %>%
#   left_join(trade_size_conversion_factor, by="BBG_Ticker") %>%
#   mutate(semiannual_SD = sd*size*conversion*sqrt(nrow(tmp)-1))
#
# #mrc calculation
# # for FX, weight = proportion of R2
# # for bond trades, weights is weighted years to R2. for tickers in bp, also multiplied by 0.0001. for bond tickers in %, multiplied by 0.01.
#
# weight <- read.csv("./scripts/trade_size_conversion_factor.csv",header=T,row.names=1)
# weight_m  <- as.matrix(weight)
#
# trades_cov_m <- as.matrix(trades_cov)
#
# sdev <- as.numeric(sqrt(t(weight_m) %*% trades_cov_m %*% weight_m))
# covarweight <- trades_cov_m %*% weight_m
# marginal_contribution <- covarweight/sdev
# total_contribution <- weight_m* covarweight/sdev
#
#
