# library(lubridate)

#' Plot asset class correlations
#' @description Downloads data from Bloomberg on major asset classes and plots their correlation
#'
#' @return Correlation plot
#' @export
#'
#' @examples \dontrun{plot_asset_cors}
plot_asset_cors <- function(){
  ticker_list <- c("AS51 Index",
                   "SPX Index",
                   "INDU Index",
                   "CCMP Index",
                   "RTY Index",
                   "SPTSX60 Index",
                   "SX5E Index",
                   "DAX Index",
                   "CAC Index",
                   "UKX Index",
                   "AEX Index",
                   "IBEX Index",
                   "SMI Index",
                   "NKY Index",
                   "HSI Index",
                   "OMX Index",
                   "STI Index",
                   "MXEF Index",
                   "IBOV Index",
                   "KOSPI Index",
                   "GTAUD10Y Govt",
                   "GTAUD3Y Govt",
                   "GTUSD30Y Govt",
                   "GTUSD10Y Govt",
                   "GTUSD2Y Govt",
                   "GTUSD5Y Govt",
                   "GTDEM30Y Govt",
                   "GTDEM10Y Govt",
                   "GTDEM5Y Govt",
                   "GTDEM2Y Govt",
                   "GTGBP10Y Govt",
                   "GTGBP2Y Govt",
                   "GTJPY10Y Govt",
                   "USDJPY Curncy",
                   "AUDUSD Curncy",
                   "EURUSD Curncy",
                   "GBPUSD Curncy",
                   "USDCHF Curncy",
                   "CADUSD Curncy")

  Rblpapi::blpConnect()
  raw_data <- Rblpapi::bdh(ticker_list, "PX_LAST", start.date=Sys.Date()-30)
  raw_data %>%
    bind_rows(.id = "Ticker") %>%
    as_tibble() ->
    bbg_data

  bbg_data %>%
    mutate(prev = lag(PX_LAST, 1), pct_ret = (PX_LAST - prev)/prev, chg = (PX_LAST - prev),
           Ticker2 = stringr::str_split_fixed(Ticker, " ", n=2)[,1]) -> bbg_data2

  bbg_data2 %>%
    filter(Ticker %in% c("AS51 Index",
                         "SPX Index",
                         "INDU Index",
                         "CCMP Index",
                         "RTY Index",
                         "SPTSX60 Index",
                         "SX5E Index",
                         "DAX Index",
                         "CAC Index",
                         "UKX Index",
                         "AEX Index",
                         "IBEX Index",
                         "SMI Index",
                         "NKY Index",
                         "HSI Index",
                         "OMX Index",
                         "STI Index",
                         "MXEF Index",
                         "IBOV Index",
                         "KOSPI Index",
                         "USDJPY Curncy",
                         "AUDUSD Curncy",
                         "EURUSD Curncy",
                         "GBPUSD Curncy",
                         "USDCHF Curncy",
                         "CADUSD Curncy")) %>%
    select(Ticker2, date, pct_ret) %>%
    rename(data = pct_ret) -> bbg_data3.1

  bbg_data2 %>%
    filter(Ticker %in% c("GTAUD10Y Govt",
                         "GTAUD3Y Govt",
                         "GTUSD30Y Govt",
                         "GTUSD10Y Govt",
                         "GTUSD2Y Govt",
                         "GTUSD5Y Govt",
                         "GTDEM30Y Govt",
                         "GTDEM10Y Govt",
                         "GTDEM5Y Govt",
                         "GTDEM2Y Govt",
                         "GTGBP10Y Govt",
                         "GTGBP2Y Govt",
                         "GTJPY10Y Govt")) %>%
    select(Ticker2, date, pct_ret) %>%
    rename(data = pct_ret) -> bbg_data3.2

  bbg_data3 <- bind_rows(bbg_data3.1, bbg_data3.2)
  bbg_data3 %>%
    tidyr::pivot_wider(names_from = Ticker2, values_from = data) %>%
    na.omit() -> tmp

  # Shorten chart axes labels
  ticker_list2 <- stringr::str_split_fixed(ticker_list, " ", n=2)[,1]

  # Calculate date analysis ran
  bbg_data %>%
    summarise(max_date = max(date)) -> run_date

  # stamp date formatting
  # sf <- lubridate::stamp("18 October 2019")

  tmp[,c(-1)] %>%
    corrr::correlate(diagonal = 1) %>%
    corrr::stretch() %>%
    ggplot(aes(forcats::fct_relevel(x, ticker_list2), forcats::fct_rev(forcats::fct_relevel(y, ticker_list2)))) +
    geom_raster(aes(fill=r)) +
    # geom_text(aes(label=round(r,2))) +
    scale_fill_gradient2(low="red3", mid="white", high = "green4", name = "Correlation") +
    labs(x="", y = "", title = paste("Rolling 1 month correlations as at", format(run_date$max_date[1], "%d %b %y"))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

}


