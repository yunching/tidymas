#-TECHNICAL ANALYSIS Toolbox---------------------------------------------------------------20090527#
# 01. Fibonacci Series                            ta.GRATIO, ta.FIB
# 02. Fibonacci Retracement/Expansion             ta.FIBPP
# 03. Alternate Price Projection                  ta.FIBAPP
# 04. Floor Trader Pivot                          ta.PIVOT
# 05. Average True Range                          ta.ATR
# 06. Simple Moving Average                       ta.SMA
# 07. Exponential Moving Average                  ta.EMA
# 08. Relative Strength Index                     ta.RSI
# 09. Stochastics                                 ta.STOCH
# 10. Bollinger Bands                             ta.BBAND
# 11. Keltner Channels                            ta.KELTC
# 12. TTM Squeeze                                 ta.TTMSQ
#-DATA (TIDY) Toolbox----------------------------------------------------------------------20190221#
# 01. Select                                      tidy.SELECT
# 02. Display                                     tidy.VIEW
# 03. Class zoo to tidy, tidy to zoo              tidy.ZOO
#--------------------------------------------------------------------------------------------------#
# TECHNICAL ANALYSIS TOOLBOX

# Fibonacci Series
ta.GRATIO = function() { (1+sqrt(5))/2 } # also known as phi

ta.FIB = function(count=20){
  fibo = array(0, c(count, 1))
  for (i in 1:count) { fibo[i,] = (ta.GRATIO()^i - (1 - ta.GRATIO())^i )/sqrt(5) }
  rownames(fibo) = 1:count
  colnames(fibo) = 'Fibo'
  fibo
}

# Fibonacci Ratios
# 1 - sqrt(phi - 1) 0.21385
# 1 - (phi - 1)     0.38197
# phi - 1           0.61803
# sqrt(phi - 1)     0.78615
# sqrt(phi)         1.27202
# phi               1.61803
# phi^2             2.61803
# phi^3             4.23607

# Fibonacci Retracement/Expansion
ta.FIBPP = function(swingpt.1, swingpt.2, fibo, sign.fib=1){
  fibo      = c('0.000','0.214','0.250','0.382','0.500','0.618','0.750','0.786','1.000',
                '1.272','1.382','1.618','2.000','2.618','4.236')
  sign.fib  = ifelse(sign.fib<=-2,-1,ifelse(sign.fib>=2,2,sign.fib))
  swing.chg = abs(swingpt.1 - swingpt.2)
  swing.ret = ifelse(sign.fib==1, min(swingpt.1,swingpt.2), max(swingpt.1,swingpt.2))
  swing.exp = ifelse(sign.fib==1, max(swingpt.1,swingpt.2), min(swingpt.1,swingpt.2))
  fib.ret   = sign.fib * as.numeric(fibo) * swing.chg + swing.ret
  fib.exp   = sign.fib * as.numeric(fibo) * swing.chg + swing.exp
  fib.px    = as.data.frame(cbind(fib.ret,fib.exp))
  rownames(fib.px) = fibo
  colnames(fib.px) = c('R.FIB','X.FIB')
  fib.px
}

# Alternate Price Projection
ta.FIBAPP = function(swingpt.1, swingpt.2, swing.alt, sign.fib=1){
  fibo      = c('0.000','0.214','0.250','0.382','0.500','0.618','0.750','0.786','1.000',
                '1.272','1.382','1.618','2.000','2.618','4.236')
  sign.fib  = ifelse(sign.fib<=-2,-1,ifelse(sign.fib>=2,2,sign.fib))
  swing.chg = abs(swingpt.1 - swingpt.2)
  fib.alt   = as.matrix(sign.fib * as.numeric(fibo) * swing.chg + swing.alt)
  rownames(fib.alt) = fibo
  colnames(fib.alt) = 'ALT.FIB'
  fib.alt
}

# Floor Trader Pivot
ta.PIVOT = function(tseries){
  ## Check Price Data Series
  if (ncol(tseries) == 4){
    tseries.hi = tseries[,2]
    tseries.lo = tseries[,3]
    tseries.cc = tseries[,4]} else
      if (ncol(tseries) == 3){
        tseries.hi = tseries[,1]
        tseries.lo = tseries[,2]
        tseries.cc = tseries[,3]} else
          stop('Price Data Series must be Open-High-Low-Close or High-Low-Close')

  date.pivot = index(tseries.cc)
  core.pivot = rbind(rep(0,3),coredata(cbind(tseries.hi,tseries.lo,tseries.cc))[1:length(date.pivot)-1,])
  core.pivot = apply(core.pivot, c(1,2), as.numeric)

  # prettymas Digits
  prettymas = function(x) {
    if ((x %% 1) != 0) prettymas.dec = nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    if ((x %% 1) == 0) prettymas.dec = 0
    return(prettymas.dec)
  }
  num.prettymas = max(max(prettymas(max(tseries)), prettymas(min(tseries))), 0)

  # Algebraic Method
  ## PivotPt = apply(dat.psd,1,mean)
  ## PivotS1 = PivotPt * 2 - dat.psd[,1]
  ## PivotR1 = PivotPt * 2 - dat.psd[,2]
  ## PivotS2 = PivotPt - (PivotR1 - PivotS1)
  ## PivotR2 = PivotPt - PivotS1 + PivotR1
  ## PivotS3 = PivotPt - (PivotR2 - PivotS1)
  ## PivotR3 = PivotPt - PivotS1 + PivotR2

  # Matrix Method
  pivot.m = t(cbind(c(6,4,2,1,-1,-2,-4), c(-3,-2,-1,1,2,4,5), c(0,1,2,1,2,1,2)))
  pivot   = round(core.pivot%*%pivot.m * 1/3, num.prettymas)
  pivot   = zoo(pivot[,7:1] ,order.by=date.pivot)
  colnames(pivot) = c('PivotS3', 'PivotS2', 'PivotS1', 'PivotPt', 'PivotR1', 'PivotR2', 'PivotR3')
  pivot
}

# Average True Range
ta.ATR = function(tseries, period, smooth='simple', wilder=TRUE){
  # smooth: simple = Simple Moving Average, exponential = Exponential Moving Average
  ## Check Price Data Series
  if (ncol(tseries) == 4){
    tseries.hi = tseries[,2]
    tseries.lo = tseries[,3]
    tseries.cc = tseries[,4]} else
      if (ncol(tseries) == 3){
        tseries.hi = tseries[,1]
        tseries.lo = tseries[,2]
        tseries.cc = tseries[,3]} else
          stop('Price Data Series must be Open-High-Low-Close or High-Low-Close')

  # prettymas Digits
  prettymas = function(x) {
    if ((x %% 1) != 0) prettymas.dec = nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    if ((x %% 1) == 0) prettymas.dec = 0
    return(prettymas.dec)
  }
  num.prettymas = max(max(prettymas(max(tseries)), prettymas(min(tseries))), 0)

  # ATR
  if (smooth=='s') smooth = 'simple'
  if (smooth=='e') smooth = 'exponential'
  true.atr = tseries.hi - tseries.lo
  true.hi  = tseries.hi - stats::lag(tseries.cc,-1)
  true.lo  = tseries.lo - stats::lag(tseries.cc,-1)
  truerg.atr = pmax(true.atr[2:length(true.atr),], pmax(true.hi, abs(true.lo)))
  if( smooth=='simple' ) {
    truerg.atr = rbind(zoo(NA, index(head(true.atr, 1))), truerg.atr)
    ATR        = rollapply(truerg.atr, period, mean, na.rm=TRUE, fill=NA, align='right')
  }
  if( smooth=='exponential' ) {
    truerg.atr = rbind(zoo(0, index(head(true.atr, 1))), truerg.atr)
    ATR        = ta.EMA(truerg.atr, 20, wilder = wilder)
  }
  sd.ATR = math.STDEV(ATR, period)
  round(cbind(ATR,sd.ATR), num.prettymas)
}

# Simple Moving Average
ta.SMA = function(tseries, period){
  # prettymas Digits
  prettymas = function(x) {
    if ((x %% 1) != 0) prettymas.dec = nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    if ((x %% 1) == 0) prettymas.dec = 0
    return(prettymas.dec)
  }
  num.prettymas = max(max(prettymas(max(tseries)), prettymas(min(tseries))), 0)

  # SMA
  round(rollapply(tseries, period, mean, na.rm=TRUE, fill=NA, align='right'), num.prettymas)
}

# Exponential Moving Average
ta.EMA = function(tseries, period, wilder=FALSE){
  ## Facilate Data Series Truncation, Dates and Core Data Seperated
  date.ema = as.Date(index(tseries))
  core.ema = coredata(tseries)
  len.ema  = length(date.ema)
  alpha.ema= ifelse(wilder==TRUE,1/period,2/(period+1))

  # prettymas Digits
  prettymas = function(x) {
    if ((x %% 1) != 0) prettymas.dec = nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    if ((x %% 1) == 0) prettymas.dec = 0
    return(prettymas.dec)
  }
  num.prettymas = max(max(prettymas(max(tseries)), prettymas(min(tseries))), 0)

  # EMA
  ## EMAtoday <- EMAprev + alpha * (close - EMAprev)
  ##          <- (1 - alpha) * EMA prev + alpha * close
  calc.ema = mean(core.ema[1:period])
  calc.ema = c(calc.ema,alpha.ema * core.ema[period+1:(len.ema-period)])
  ema      = stats::filter(calc.ema, (1 - alpha.ema), method = 'recursive')
  ema      = c(rep(NA,period-1),ema)
  round(zoo(ema, order.by = date.ema), num.prettymas)
}

# Relative Strength Index
ta.RSI = function(tseries, period){
  if (is.null(ncol(tseries)) != TRUE) stop('tseries use Close only.')

  # RSI
  date.rsi = as.Date(index(tseries))
  calc.rsi = diff(tseries)
  up.rsi   = ifelse(calc.rsi>0,calc.rsi,0)
  dn.rsi   = ifelse(calc.rsi<0,abs(calc.rsi),0)
  avgup.rsi= ta.EMA(up.rsi, period, wilder=TRUE)
  avgdn.rsi= ta.EMA(dn.rsi, period, wilder=TRUE)

  rs       = avgup.rsi/avgdn.rsi
  rsi      = round(100 - 100*(1/(1+rs)), 2)
  zoo(c(NA,rsi), order.by = date.rsi)
}

# Stochastics - Fast K, Fast D, Slow D
ta.STOCH = function(tseries, period.k, period.d, period.slowd, smooth.k=3){
  ## Check Price Data Series
  if (ncol(tseries) == 4){
    tseries.hi = tseries[,2]
    tseries.lo = tseries[,3]
    tseries.cc = tseries[,4]} else
      if (ncol(tseries) == 3){
        tseries.hi = tseries[,1]
        tseries.lo = tseries[,2]
        tseries.cc = tseries[,3]} else
          stop('Price Data Series must be Open-High-Low-Close or High-Low-Close')

  # Stochastics
  minlo.stoch= rollapply(tseries.lo,period.k, min, fill=NA, align='right')
  maxhi.stoch= rollapply(tseries.hi, period.k, max, fill=NA, align='right')
  num.stoch  = (tseries.cc - minlo.stoch)
  dem.stoch  = (maxhi.stoch - minlo.stoch)

  ## Internal Smoothing of K
  ## for smooth.k = 1, no internal smoothing of K; default = 3
  num.stoch  = rollapply(num.stoch, smooth.k, mean, na.rm=TRUE, fill=NA, align='right')
  dem.stoch  = rollapply(dem.stoch, smooth.k, mean, na.rm=TRUE, fill=NA, align='right')

  fastK = num.stoch/dem.stoch * 100
  fastD = rollapply(fastK, period.d, mean, na.rm=TRUE, fill=NA, align='right')
  slowD = rollapply(fastD, period.slowd, mean, na.rm=TRUE, fill=NA, align='right')
  stoch = cbind(fastK, fastD, slowD)
  stoch[which(stoch<0)]=0
  stoch[which(stoch>100)]=100
  round(stoch, 2)
}

# Bollinger Bands
ta.BBAND = function(tseries, period, stdev=2){
  # prettymas Digits
  prettymas = function(x) {
    if ((x %% 1) != 0) prettymas.dec = nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    if ((x %% 1) == 0) prettymas.dec = 0
    return(prettymas.dec)
  }
  num.prettymas = max(max(prettymas(max(tseries)), prettymas(min(tseries))), 0)

  if (is.null(ncol(tseries)) != TRUE) stop('tseries use Close only.')

  # Bollinger Bands
  mid.band = rollapply(tseries, period, mean, na.rm=TRUE, fill=NA, align='right')
  sdev.band= math.STDEV(tseries, period, type='s') * stdev
  up.bband = mid.band + sdev.band
  dn.bband = mid.band - sdev.band
  round(cbind(dn.bband, mid.band, up.bband), num.prettymas)
}

# Keltner Channels
ta.KELTC = function(tseries, period, num.ATR=1.5, smooth='simple', wilder=TRUE){
  ## Check Price Data Series
  if (ncol(tseries) == 4){
    tseries.hi = tseries[,2]
    tseries.lo = tseries[,3]
    tseries.cc = tseries[,4]} else
      if (ncol(tseries) == 3){
        tseries.hi = tseries[,1]
        tseries.lo = tseries[,2]
        tseries.cc = tseries[,3]} else
          stop('Price Data Series must be Open-High-Low-Close or High-Low-Close')

  # prettymas Digits
  prettymas = function(x) {
    if ((x %% 1) != 0) prettymas.dec = nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    if ((x %% 1) == 0) prettymas.dec = 0
    return(prettymas.dec)
  }
  num.prettymas = max(max(prettymas(max(tseries)), prettymas(min(tseries))), 0)

  # Keltner Channel
  mid.band = rollapply(tseries.cc, period, mean, na.rm=TRUE, fill=NA, align='right')
  shift.kc = ta.ATR(cbind(tseries.hi,tseries.lo,tseries.cc), period, smooth, wilder)[,1] * num.ATR
  up.keltc = mid.band + shift.kc
  dn.keltc = mid.band - shift.kc
  round(cbind(dn.keltc, mid.band,up.keltc), num.prettymas)
}

# TTM Squeeze
ta.TTMSQ = function(tseries, period, stdev=2, num.ATR=1.5, smooth='simple', wilder=TRUE){
  ## Check Price Data Series
  if (ncol(tseries) == 4){
    tseries.hi = tseries[,2]
    tseries.lo = tseries[,3]
    tseries.cc = tseries[,4]} else
      if (ncol(tseries) == 3){
        tseries.hi = tseries[,1]
        tseries.lo = tseries[,2]
        tseries.cc = tseries[,3]} else
          stop('Price Data Series must be Open-High-Low-Close or High-Low-Close')

  # prettymas Digits
  prettymas = function(x) {
    if ((x %% 1) != 0) prettymas.dec = nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    if ((x %% 1) == 0) prettymas.dec = 0
    return(prettymas.dec)
  }
  num.prettymas = max(max(prettymas(max(tseries)), prettymas(min(tseries))), 0)

  # TTM Squeeze
  date.sq     = as.Date(index(tseries.cc))
  bband.sq    = ta.BBAND(tseries.cc, period, stdev)
  keltc.sq    = ta.KELTC(cbind(tseries.hi,tseries.lo,tseries.cc), period, num.ATR, smooth, wilder)
  up.bband.sq = bband.sq[,3]
  dn.bband.sq = bband.sq[,1]
  up.keltc.sq = keltc.sq[,3]
  dn.keltc.sq = keltc.sq[,1]

  minlo.sq = rollapply(tseries.lo, period, min, fill=NA, align='right')
  maxhi.sq = rollapply(tseries.hi, period, max, fill=NA, align='right')
  ema.ttmsq= ta.EMA(tseries.cc, period)

  delta.sq = tseries.cc - ((minlo.sq+maxhi.sq)/2 + ema.ttmsq)/2
  ind.sq   = (1:length(delta.sq))
  core.sq  = coredata(delta.sq)
  histSQ = delta.sq * 0 # zeroise vector for output
  # Squeeze Histogram using Loop
  ## for (i in period:length(delta.sq)){
  ##   histSQ[i] = as.numeric(tail(lm(core.sq ~ ind.sq, subset=(i-period+1):(i))$fitted.values,1))}
  # Squeeze Histogram using Efficient Algorithim
  histSQ = math.LINREG(core.sq~ind.sq, data=na.omit(as.data.frame(cbind(core.sq, ind.sq))), period)
  histSQ = round(zoo(c(rep(NA,length(tseries.cc)-length(histSQ)),histSQ)
                     , order.by = as.Date(index(tseries.cc))), num.prettymas)
  # Squeeze Volatiltity Expansion/Contraction
  ttmsq = ifelse((up.keltc.sq-dn.keltc.sq)==0,0,(up.bband.sq-dn.bband.sq)/(up.keltc.sq-dn.keltc.sq))
  dotSQ = ifelse(ttmsq<1,0,1)
  cbind(dotSQ, histSQ)
}

#--------------------------------------------------------------------------------------------------#
# MATHEMATICS TOOLBOX

# Mean
math.MEAN = function(vseries, type='arithmetic'){
  switch(type,
         arithmetic = mean(vseries),
         geometric  = exp(mean(log(vseries))),
         harmonic   = 1/mean(1/vseries),
         stop('Selection of Mean ~ Arithmetic, Geometric, Harmonic')
  )
}

# Standard Deviation for Time Series
math.STDEV = function(tseries, period, type='p'){
  ## type: 'p' as variance popn, 's' as variance sample
  if (type!='p' && type !='s') type = 'p'
  date.stats = as.Date(index(tseries))
  core.stats = coredata(tseries)
  wt.stats   = rep(1/period, period)
  mean.stats = as.vector(stats::filter(core.stats, wt.stats, method="convolution", side=1))
  ## Variance(x) <- E(x^2) - E(x)^2
  if (type=='p') sdev.factor = (period-1) / period
  if (type=='s') sdev.factor = 1
  var.stats  = stats::filter(core.stats^2, wt.stats, method="convolution", side=1) - mean.stats^2
  sdev.stats = as.vector(suppressWarnings(suppressMessages(sqrt(var.stats * sdev.factor))))
  sdev.stats[which(sdev.stats == 'NaN')] = 0
  zoo(sdev.stats, date.stats)
}

# Moments of Distribution for Time Series
math.MOMENTS = function(tseries, by.month=FALSE, by.return=TRUE){
  ## Required Package
  pkg.attach = c('xts', 'moments')
  pkg.exist = TRUE
  if (all(pkg.attach %in% (.packages()))==FALSE) {
    pkg.detach = pkg.attach[which(!pkg.attach %in% (.packages()))]
    pkg.attach = suppressWarnings(suppressMessages(lapply(pkg.attach, require, character.only=TRUE)))
    pkg.exist=FALSE
  }

  ## Check Price Data Series
  if (ncol(tseries) == 4){ cc.dat = as.xts(tseries[,4])} else
    if (ncol(tseries) == 3){ cc.dat = as.xts(tseries[,3])} else
      stop('Price Data Series must be Open-High-Low-Close or High-Low-Close')

  ## Price/Price Returns
  if (by.return == TRUE){ cc.mom = diff(log(cc.dat), fill=NA) } else cc.mom = cc.dat

  beg.yr = as.numeric(format(index(cc.mom)[1], '%Y'))
  end.yr = as.numeric(format(index(cc.mom)[length(cc.mom)], '%Y'))
  if (by.month == TRUE){ row.mom = format(index(to.monthly(cc.dat)), '%Y%m%') } else
    row.mom = beg.yr:end.yr
  moments= matrix(rep(0,length(row.mom)), nrow=length(row.mom), ncol=2)
  rownames(moments) = row.mom
  colnames(moments) = c('SKEW', 'KURT')
  for (i in 1:length(row.mom)){
    moments[i,1] = skewness(cc.mom[as.character(row.mom[i])], na.rm=TRUE)
    moments[i,2] = kurtosis(cc.mom[as.character(row.mom[i])], na.rm=TRUE)
  }

  if (pkg.exist==FALSE) pkg.detach = suppressWarnings(suppressMessages(lapply(paste('package', pkg.detach, sep=':'), detach, character.only=TRUE)))
  moments
}

# Roots of a Function
math.ROOTS = function (f, interval, lower=min(interval), upper=max(interval),
                       tol=.Machine$double.eps^0.2, maxiter=1000, n=100, ...){
  ## Example
  ## math.ROOTS(function(x) sin(x), c(-10,10))
  if (!missing(interval) && length(interval) != 2)
    stop("'interval' must be a vector of length 2")
  if (!is.numeric(lower) || !is.numeric(upper) || lower >= upper)
    stop("lower < upper is not fulfilled")
  xseq = seq(lower, upper, len=n+1)
  mod  = f(xseq, ...)
  equi = xseq[which(mod == 0)]
  ss   = mod[1:n] * mod[2:(n + 1)]
  ii   = which(ss < 0)
  for (i in ii) equi = c(equi, uniroot(f, lower=xseq[i], upper=xseq[i + 1], ...)$root)
  return(equi)
}

# Moving Window Linear Regression
math.LINREG = function(formula, data, width, ...){
  ## Adapted from Douglas Bates on Moving Window Regression
  ## http://tolstoy.newcastle.edu.au/R/help/04/04/1269.html
  ## https://stat.ethz.ch/pipermail/r-sig-finance/2004q3/000086.html
  mCall      = match.call()
  mCall$width= NULL
  mCall[[1]] = as.name("lm")
  mCall$x    = mCall$y = TRUE
  bigfit     = eval(mCall, parent.frame())
  ncoef      = length(coef(bigfit))
  nr         = nrow(data)
  width      = as.integer(width)[1]
  stopifnot(width >= ncoef, width <= nr)
  y    = bigfit$y
  x    = bigfit$x
  terms= bigfit$terms
  inds = embed(seq(nr), width)[, rev(seq(width))]
  sumrys <- lapply(seq(nrow(inds)),
                   function(st) {
                     ind = inds[st,]
                     fit = lm.fit(x[ind,], y[ind])
                     fit$terms = terms
                     class(fit) = "lm"
                     fit
                   })
  sapply(sumrys, function(sm) tail(fitted.values(sm),1))
}

#--------------------------------------------------------------------------------------------------#
# DATA (TIDY) TOOLBOX

# Data Series Selection tidy Time Series
tidy.SELECT = function(tidydata, fname=NULL){
  tidydata %>% dplyr::filter(Ticker==fname) %>% unnest() %>% select(-Ticker)
}

# Data Series Display tidy Time Series
tidy.VIEW = function(tidydata, type='head', rows=10){
  if (type=='h') type = 'head'
  if (type=='t') type = 'tail'
  if (type!='head' && type!='tail') stop('type must be defined as head or tail')
  if (type=='head') view = head(tidydata, rows)
  if (type=='tail') view = tail(tidydata, rows)
  view %>% print.data.frame
}

# Data Series Class zoo to tidy, tidy to zoo
tidy.ZOO = function(tseries, zoo=FALSE, tidy=FALSE, tname='Value'){
  if(zoo==FALSE && tidy==FALSE) stop('zoo or tidy must be defined as TRUE')
  if(zoo==TRUE && is.null(colnames(tseries))==FALSE) {
    dseries = tseries %>% broom::tidy() %>%  spread(series, value) %>% rename(date = index) %>%
      select(date, colnames(tseries)) }
  if(zoo==TRUE && is.null(colnames(tseries))==TRUE) {
    dseries = tseries %>% broom::tidy() %>%  spread(series, value) %>% rename(date = index) %>%
      select(date, 'x')
    tidy.name = colnames(dseries)
    colnames(dseries) = c(tidy.name[!tidy.name %in% "x"], tname)
  }
  if(tidy==TRUE) dseries = tseries[2:length(colnames(tseries))] %>% zoo(order.by=tseries$date)
  return(dseries)
}

#--------------------------------------------------------------------------------------------------#
# PORTFOLIO TOOLBOX

# Sharpe
calc_sharpe <- function(ret, annualize_factor = 252) {
  mean_ret <- mean(ret, na.rm = T)
  sd_ret <- sd(ret, na.rm = T)

  (mean_ret * annualize_factor)/(sd_ret * sqrt(annualize_factor))
}

# Sortino
calc_sortino = function(ret, annualize_factor = 252) {
  mean_ret = mean(ret, na.rm = T)
  ret = ifelse(ret < 0, ret,0)
  sd_downside = sd(ret, na.rm = T)
  (mean_ret * annualize_factor)/(sd_downside * sqrt(annualize_factor))
}
