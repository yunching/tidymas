
ohlc <- function(sec_list){
  fields <- c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST")
  Rblpapi::bdp(sec_list, fields)
}
