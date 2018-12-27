#' @importFrom Rblpapi bdh bdp

NULL

#' Asset class rebalancing
#'
#' @param port.SGD Size of portfolio in Singapore dollars
#' @param weight percentage allocation to asset class
#' @param units Existing units of asset class
#'
#' @return Number of units to add to portfolio (-ve means reduce allocation)
#' @export
#'
#' @examples
#' \donttest{
#' eqd(1000000, 0.05, 10)
#' }
eqd <- function(port.SGD, weight, units) {
  SGD <- bdp("SGD WMCD Curncy","PX_LAST")
  index <- bdp("M1CXDBKB Index","PX_LAST")
  R2.USD <- port.SGD/SGD[1,1]
  eqd.not <- units * index[1,1]
  weight <- ifelse(weight > 1, weight/100, weight)
  buy.not <- (weight *R2.USD) - eqd.not
  buy.unit <- buy.not/index[1,1]

  return(buy.unit)
}
