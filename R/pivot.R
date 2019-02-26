#' Function for Pivot Points
#'
#' @param tseries dataframe contain columns date, open, high, low, close
#'
#' @return dataframe containing pivots
#' @export
#'
#' @examples
#' library(dplyr)
#' data(swat_data)
#' df_rx <- swat_data %>% dplyr::filter(Ticker == "RX1 Comdty") %>% dplyr::select(-Ticker)
#' en.pivot(df_rx)
en.pivot <- function(tseries) {
  ## Check Price Data Series
  if (ncol(tseries) == 5){
    tseries.hi = tseries[,3]
    tseries.lo = tseries[,4]
    tseries.cc = tseries[,5]
  } else if (ncol(tseries) == 4){
    tseries.hi = tseries[,2]
    tseries.lo = tseries[,3]
    tseries.cc = tseries[,4]
  } else {

    stop('Price Data Series must be Date-Open-High-Low-Close or Date-High-Low-Close')
  }

  ctr  <- (tseries.hi + tseries.lo + tseries.cc)/3
  R1   <- 2*ctr - tseries.lo
  R2   <- ctr + (tseries.hi - tseries.lo)
  R3   <- ctr + 2*(tseries.hi - tseries.lo)
  S1   <- 2*ctr - tseries.hi
  S2   <- ctr - (tseries.hi - tseries.lo)
  S3   <- ctr - 2*(tseries.hi - tseries.lo)

  data.frame(date = tseries[,1], R1, R2, R3, ctr, S1, S2, S3)
}

