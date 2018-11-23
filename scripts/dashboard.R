library(Rblpapi)
blpConnect()

futures_list <- c("RX1 Comdty", "G 1 Comdty", "IK1 Comdty ")
sec_list <- c("GTDEM10Y Govt", "GTGBP10Y Govt")
fields <- c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST")

bdp(sec_list, fields)
