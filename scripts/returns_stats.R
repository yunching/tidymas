source("scripts/pmad_toolkit.R")

# Statistical properties --------------------------------------------------
stats_property <- function(ticker_list, returns_db, start_date, end_date){
  #calculate returns

  returns_db %>%
    filter(BBG_Ticker %in% {{ ticker_list }}) %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(quarter = paste(lubridate::year(date), "Q", lubridate::quarter(date), sep="")) %>%
    ungroup %>%
    group_by(BBG_Ticker, quarter) %>%
    mutate(ret_index = cumprod(1 + daily_return),
           prev_peak = cummax(ret_index),
           draw_down = (ret_index - prev_peak)/prev_peak
           ) %>%
    # select(BBG_Ticker, quarter, daily_return, Close, ret_index) %>%
    summarise(last = last(Close),
              last_ret = last(daily_return),
              min = min(daily_return),
              max = max(daily_return),
              max_min_ratio = abs(max / min),
              mean = mean(daily_return),
              median = median(daily_return),
              sd = sd(daily_return),
              skew = moments::skewness(daily_return),
              kurtosis = moments::kurtosis(daily_return),
              sd1_move = last * sd,
              sd2_move = last * sd * 2,
              max_dd = min(draw_down),
              .groups = "keep")

}

stats_property(c("SPX Index"), trades_final, start_date = ymd(20200401), end_date = ymd(20200930)) %>% view()


returns_hist <- function(ticker_list, returns_db){
  returns_db %>%
    filter(BBG_Ticker %in% {{ ticker_list }}) %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(quarter = paste(lubridate::year(date), "Q", lubridate::quarter(date), sep="")) %>%
    ungroup %>%
    group_by(BBG_Ticker, quarter) %>%
    select(BBG_Ticker, quarter, daily_return)
}

trades_final %>%
  filter(BBG_Ticker == "SPX Index") %>%
  ggplot(aes(x=daily_return)) +
  geom_histogram(aes(y = ..density..)) + stat_function(fun=dnorm, args = with(mtcars, c(mean = mean(mpg), sd = sd(mpg))))


# ggplot(mtcars, aes(x = mpg)) +
#   geom_histogram(aes(y = ..density..), fill = "red") +
#   stat_function(
#     fun = dnorm,
#     args = with(mtcars, c(mean = mean(mpg), sd = sd(mpg)))
#   ) +
#   scale_x_continuous("Miles per gallon") +
#   opts(title = "Histogram with Normal Curve")
