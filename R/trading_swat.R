#'swat_data
#'@name swat_data
#'@docType data
#'@details data for trading swat development
NULL

# Technical indicators
ema <- function(ts, window){
  TTR::EMA(ts, n = window)
}

lala <- function(){
  message("Hi")
}
