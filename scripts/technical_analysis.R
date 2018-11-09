library(zoo)

ema <- function(ts, period=20, wilder=FALSE){
  ema.date <- zoo::as.Date(index(ts))
  ema.core <- zoo::coredata(ts)
  ema.len <- length(ema.date)
  ema.alpha <- ifelse(wilder==TRUE, 1/period, 2/(period + 1))

  ema.calc <- mean(ema.core[1:period])
  ema.calc <- c(ema.calc, ema.alpha * ema.core[(period + 1):(ema.len - period)])
  ema <- filter(ema.calc, (1 - ema.alpha), method="recursive")
  ema <- c(rep(NA, period-1), ema)

  return(zoo(ema, ema.date))
}

rsi <- function(ts, period=14){
  rsi.date <- zoo::as.Date(index(ts))
  rsi.calc <- diff(ts)
  rsi.up  <- ifelse(rsi.calc > 0, rsi.calc, 0)
  rsi.dn   <- ifelse(rsi.calc < 0, abs(rsi.calc), 0)
  rsi.avgup <- ema(rsi.up, period, wilder=TRUE)
  rsi.avgdn <- ema(rsi.dn, period, wilder=TRUE)

  rs <- rsi.avgup/rsi.avgdn
  rsi <-  c(NA, 100 - 100 * (1 / (1 + rs)))


  return(zoo(rsi, rsi.date))
}

stoc <- function(ts, per.k=14, per.fastd=3, per.slowd=3, smooth.k=1, debug=FALSE){
  stoc.date <- zoo::as.Date(index(ts))
  if (!is.array(ts)){
    stoc.high <- ts
    stoc.low <- ts
    stoc.close <- ts
  }
  else if (ncol(ts) == 4) {
    stoc.high <- ts[, 2]
    stoc.low <- ts[, 3]
    stoc.close <- ts[, 4]
  }
  else if (ncol(ts) == 3) {
    stoc.high <- ts[, 1]
    stoc.low <- ts[, 2]
    stoc.close <- ts[, 3]
  }
  else if (ncol(ts) == 1) {
    stoc.high <- ts
    stoc.low <- ts
    stoc.close <- ts
  }
  else stop("Price series must be either Open-High-Low-Close, High-Low-Close, or Close")

  stoc.lowestlow <- rollapply(stoc.low, per.k, min, fill=NA, align = 'right')
  stoc.highesthigh <- rollapply(stoc.high, per.k, max, fill=NA, align = 'right')
  #pad NAs with cummaxs and cummins to get true lowest low and highest high
  stoc.lowestlow[1:(per.k - 1)] <- cummin(stoc.low)[1:(per.k - 1)]
  stoc.highesthigh[1:(per.k - 1)] <- cummax(stoc.high)[1:(per.k - 1)]

  stoc.num  <- (stoc.close - stoc.lowestlow)
  stoc.den  <- (stoc.highesthigh - stoc.lowestlow)

  if (smooth.k > 1){
    stoc.num <- rollapply(stoc.num, smooth.k, mean, na.rm=TRUE, fill=NA, align='right')
    stoc.den <- rollapply(stoc.den, smooth.k, mean, na.rm=TRUE, fill=NA, align='right')
  }

  fastK <- stoc.num / stoc.den * 100
  #deliberately void earlier fast ks before per.k
  fastK[1:(per.k + smooth.k - 2)] <- NA
  fastD <- rollapply(fastK, per.fastd, mean, na.rm=TRUE, fill=NA, align='right')
  fastD[1:(per.k + smooth.k + per.fastd - 3)] <- NA
  slowD <- rollapply(fastD, per.slowd, mean, na.rm=TRUE, fill=NA, align='right')
  slowD[1:(per.k + smooth.k + per.fastd + per.slowd - 4)] <- NA
  if (debug==TRUE){
    stoc <-
      cbind(
        stoc.high,
        stoc.low,
        stoc.close,
        stoc.lowestlow,
        stoc.highesthigh,
        stoc.num,
        stoc.den,
        fastK,
        fastD,
        slowD
      )
  }
  else{
    stoc <- cbind(fastK, fastD, slowD)
  }

  return(zoo(stoc, stoc.date))
}

library(TTR) #for checking my calcs versus a ready package
library(microbenchmark) #for benchmarking against TTR
data(ttrc) #load dummmy timeseries to apply TA functions
x <- ttrc[1:30,"Close"]

# Check output versus TTR functions + benchmarking
all.equal(TTR::EMA(x, 20), coredata(ema(x, 20)))
microbenchmark(TTR::EMA(x, 20), ema(x, 20))

all.equal(TTR::RSI(x, 20), coredata(rsi(x, 20)))
microbenchmark(TTR::RSI(x, 20), rsi(x, 20))

all.equal(TTR::stoch(x, smooth = 1), coredata(stoc(x)) / 100)
microbenchmark(TTR::stoch(x, smooth = 1), stoc(x))
all.equal(TTR::stoch(x, smooth = 3), coredata(stoc(x, smooth.k = 3)) / 100)
microbenchmark(TTR::stoch(x, smooth = 3), stoc(x, smooth.k = 3))
