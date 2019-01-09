library(DBI)
library(odbc)
library(Rblpapi)
library(tidyverse)

blpConnect()
con <- dbConnect(odbc(), "doyc")

sec_list <- c(
  "GTAUD10Y Govt",
  "GTBEF10Y Govt",
  "GTCAD10Y Govt",
  "GTFRF10Y Govt",
  "GTDEM10Y Govt",
  "GTITL10Y Govt",
  "GTJPY10Y Govt",
  "GTNLG10Y Govt",
  "GTKRW10Y Govt",
  "GTESP10Y Govt",
  "GTGBP10Y Govt",
  "GT10 Govt",
  "GTDKK10Y Govt",
  "GTATS10Y Govt",
  "GTPTE10Y Govt",
  "GTIEP10Y Govt",
  "GTMXN10Y Govt",
  "GTSEK10Y Govt",
  "GTNOK10Y Govt",
  "GTTHB10Y Govt",
  "GTPLN10Y Govt",
  "GTFIM10Y Govt",
  "GTNZD10Y Govt",
  "GTCHF10Y Govt",
  "GTHKD10Y Govt",
  #Equity indices
  "SPX Index"
)

bdh_data <- bdh(sec_list, c("PX_LAST", "VOLUME"), start.date=Sys.Date()- 5 * 365)

data_pack <- bdh_data %>%
  bind_rows(.id = "Ticker") %>%
  as_tibble() %>%
  transmute(Ticker, Date = lubridate::ymd(date), PX_LAST, VOLUME, Source = "BBG")

data <- dbWriteTable(con, "bdh", data_pack, overwrite = TRUE)

bdp_data <- bdp(sec_list, c("NAME", "SECURITY_NAME", "SHORT_NAME"))

data <- dbWriteTable(con, "bdp", bdp_data, overwrite = TRUE)
