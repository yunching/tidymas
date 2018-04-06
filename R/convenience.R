library(tidyverse)
library(grid)
library(gridExtra)
library(Rblpapi)
library(MacrobondAPI)
library(xts)
library(lubridate)
library(forecast)
library(ggfortify)

#Establish connection with Bloomberg
blpConnect()

getData <- function(ticker, start_date = Sys.Date()-365){
  bbg_data <- bdh(ticker, c("PX_LAST"), start.date = start_date)
}

getData_xts <- function(ticker, start_date = Sys.Date()-365){
  bbg_data <- bdh(ticker, c("PX_OPEN", "PX_HIGH","PX_LOW","PX_LAST"), start.date = start_date)
  my_ts <- xts(cbind(bbg_data$PX_OPEN, bbg_data$PX_HIGH, bbg_data$PX_LOW, bbg_data$PX_LAST), order.by = bbg_data$date)
  colnames(my_ts) <- c("Open", "High", "Low", "Close")
  my_ts
}
#tmp <- getData_xts("GBPUSD Curncy")

ggTS <- function(ticker, title = ticker, yield_mode = FALSE, start_date = Sys.Date()-365){
  data <- getData(ticker = ticker, start_date = start_date)
  if (yield_mode == FALSE){
    my_subtitle <- paste("Last:", format(round(data$PX_LAST[length(data$PX_LAST)], digits = 2), big.mark = ","),
                         "1D ret:", paste0(format(round((data$PX_LAST[length(data$PX_LAST)] / data$PX_LAST[length(data$PX_LAST) - 1] - 1) * 100, 2), nsmall = 2), "%"),
                         "L:", format(round(min(data$PX_LAST), digits = 2), big.mark = ","),
                         "H:", format(round(max(data$PX_LAST), digits = 2), big.mark = ",")
    )
    ggplot2::ggplot(data, ggplot2::aes(x = date, y = PX_LAST)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(colour = "blue") +
      ggplot2::labs(title = title, y = "Price", x = "Date", subtitle = my_subtitle)
  }
  else {
    my_subtitle <- paste("Last:", data$PX_LAST[length(data$PX_LAST)],
                         "1D yld chg:", paste0(format(round(data$PX_LAST[length(data$PX_LAST)] - data$PX_LAST[length(data$PX_LAST)-1] , 2), nsmall = 3), "%"),
                         "L:", format(round(min(data$PX_LAST), digits = 2), big.mark = ","),
                         "H:", format(round(max(data$PX_LAST), digits = 2), big.mark = ",")
    )
    ggplot2::ggplot(data, ggplot2::aes(x = date, y = PX_LAST)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(colour = "blue") +
      ggplot2::labs(title = title, y = "Yield", x = "Date", subtitle = my_subtitle)
  }


}

getMB <- function(mb_ticker){
  my_data <- FetchOneTimeSeries(mb_ticker)
  if (getIsError(my_data))
    stop(getErrorMessage(my_data))

  as.xts(my_data)
}

ggXTS <- function(my_xts, title = "Value", series_subset = ""){
  if (length(colnames(my_xts)) > 1){
    stop("Multiple data series detected in plot.")
  }

  subtitle <- paste("Last:", round(my_xts[length(my_xts), 1], digits = 2),
                    paste0("(",format(index(my_xts)[length(my_xts)], "%b-%y"), ")"),
                    "Prev:", round(my_xts[length(my_xts) - 1, 1], digits = 2),
                    "Chg:", format(round((coredata(my_xts[length(my_xts), 1]) - coredata(my_xts[length(my_xts) - 1, 1])), digits = 2), nsmall = 2)
  )

  #, ts.colour = 'dodgerblue3'
  #autoplot(my_xts) + labs(title = title, subtitle = subtitle, y = "Index", x = "Date")
  ggplot(my_xts, aes(x = Index, y = my_xts[,1])) + geom_line(col = "dodgerblue3")  + labs(title = title, subtitle = subtitle, y = "Index", x = "Date")
}

m_yoy <- function(series){
  result <- (series / lag(series, 12) - 1) * 100
  na.omit(result)
}

m_mom <- function(series){
  result <- (series / lag(series, 1) - 1) * 100
  na.omit(result)
}

q_qoq <- function(series){
  result <- (series / lag(series, 1) - 1) * 100
  na.omit(result)
}

q_yoy <- function(series){
  result <- (series / lag(series, 4) - 1) * 100
  na.omit(result)
}

y_yoy <- function(series){
  result <- (series / lag(series, 1) - 1) * 100
  na.omit(result)
}

hline <- function(y_val){
  geom_hline(yintercept = y_val, color = "black", linetype = "dashed")
}

blank_chart <- function(){
  grid.rect(gp=gpar(col="white"), draw = FALSE)
}
