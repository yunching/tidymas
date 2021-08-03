source("scripts/pmad_toolkit.R")
blpConnect()

start_date <- ymd(20101231)
end_date <- ymd(20210630)

# Statistical properties --------------------------------------------------
stats_property <- function(ticker_list, returns_db, start_date, end_date){
  #calculate returns

  returns_db %>%
    filter(BBG_Ticker %in% {{ ticker_list }}) %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(quarter = paste(lubridate::year(date), "Q", lubridate::quarter(date), sep="")) %>%
    ungroup %>%
    group_by(BBG_Ticker, quarter) %>%
    mutate(ret_index = cumprod(1 + period_return),
           prev_peak = cummax(ret_index),
           draw_down = (ret_index - prev_peak)/prev_peak
           ) %>%
    # select(BBG_Ticker, quarter, period_return, Close, ret_index) %>%
    summarise(last = last(Close),
              last_ret = last(period_return),
              min = min(period_return),
              max = max(period_return),
              max_min_ratio = abs(max / min),
              mean = mean(period_return),
              median = median(period_return),
              sd = sd(period_return),
              skew = moments::skewness(period_return),
              kurtosis = moments::kurtosis(period_return),
              sd1_move = last * sd,
              sd2_move = last * sd * 2,
              max_dd = min(draw_down),
              .groups = "keep")

}

stats_property_full <- function(ticker_list, returns_db, start_date, end_date){
  #calculate returns

  returns_db %>%
    filter(BBG_Ticker %in% {{ ticker_list }}) %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(quarter = paste(lubridate::year(date), "Q", lubridate::quarter(date), sep="")) %>%
    ungroup %>%
    group_by(BBG_Ticker) %>%
    mutate(ret_index = cumprod(1 + period_return),
           prev_peak = cummax(ret_index),
           draw_down = (ret_index - prev_peak)/prev_peak,
           quarter = "Full"
    ) %>%
    # select(BBG_Ticker, quarter, period_return, Close, ret_index) %>%
    summarise(last = last(Close),
              last_ret = last(period_return),
              min = min(period_return),
              max = max(period_return),
              max_min_ratio = abs(max / min),
              mean = mean(period_return),
              median = median(period_return),
              sd = sd(period_return),
              skew = moments::skewness(period_return),
              kurtosis = moments::kurtosis(period_return),
              sd1_move = last * sd,
              sd2_move = last * sd * 2,
              max_dd = min(draw_down),
              .groups = "keep")

}

stats_data <- fetch_bbg_data(c("SPX Index", "UKX Index", "SX5E Index", "NKY Index", "MXWO Index", "LEGATRUU Index"),
                             start_date = start_date,
                             end_date = end_date,
                             opt = c("CDR"="5D"))
stats_return_type <- tribble(
  ~BBG_Ticker, ~Return_type,
  "SPX Index", "Price",
  "UKX Index", "Price",
  "SX5E Index", "Price",
  "NKY Index", "Price",
  "MXWO Index", "Price",
  "LEGATRUU Index", "Price"
)

stats_returns <- add_returns(stats_data, stats_return_type)
stats <- stats_property(c("SPX Index", "UKX Index", "SX5E Index", "NKY Index", "MXWO Index", "LEGATRUU Index"), stats_returns, start_date = start_date, end_date = end_date)
stats_full <- stats_property_full(c("SPX Index", "UKX Index", "SX5E Index", "NKY Index", "MXWO Index", "LEGATRUU Index"), stats_returns, start_date = ymd(20160630), end_date = end_date)

stats %>% write_csv("return_stats.csv")

stats_full %>% write_csv("return_stats_full.csv")

returns_hist <- function(ticker_list, returns_db){
  returns_db %>%
    filter(BBG_Ticker %in% {{ ticker_list }}) %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(quarter = paste(lubridate::year(date), "Q", lubridate::quarter(date), sep="")) %>%
    ungroup %>%
    group_by(BBG_Ticker, quarter) %>%
    select(BBG_Ticker, quarter, period_return)
}

tmp <- trades_final %>%
  filter(BBG_Ticker == "SPX Index") %>%
  ungroup %>%
  select(period_return)

ggplot(tmp, aes(x=period_return)) +
  # ggplot(aes(x=period_return)) +
  geom_histogram(aes(y=..density..), binwidth = 0.02, colour="black", fill="white")

# +
#   geom_density()


# ggplot(mtcars, aes(x = mpg)) +
#   geom_histogram(aes(y = ..density..), fill = "red") +
#   stat_function(
#     fun = dnorm,
#     args = with(mtcars, c(mean = mean(mpg), sd = sd(mpg)))
#   ) +
#   scale_x_continuous("Miles per gallon") +
#   opts(title = "Histogram with Normal Curve")
