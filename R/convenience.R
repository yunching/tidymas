library(tidyverse)
library(grid)
library(gridExtra)
library(Rblpapi)
library(MacrobondAPI)
library(xts)
library(lubridate)
library(forecast)

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
    my_subtitle <- paste("Low:", format(min(data$PX_LAST), big.mark = ","),
                         "High:", format(max(data$PX_LAST), big.mark = ","),
                         "Last:", format(data$PX_LAST[-1], big.mark = ","),
                         "1D return:", paste0(format(round((data$PX_LAST[length(data$PX_LAST)] / data$PX_LAST[length(data$PX_LAST) - 1] - 1) * 100, 2), nsmall = 2), "%")
    )
    ggplot2::ggplot(data, ggplot2::aes(x = date, y = PX_LAST)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(colour = "blue") +
      ggplot2::labs(title = title, y = "Price", x = "Date", subtitle = my_subtitle)
  }
  else {
    my_subtitle <- paste("Low:", min(data$PX_LAST),
                         "High:", max(data$PX_LAST),
                         "Last:", data$PX_LAST[-1],
                         "1D yld chg:", paste0(format(round(data$PX_LAST[length(data$PX_LAST)] - data$PX_LAST[length(data$PX_LAST)-1] , 2), nsmall = 3), "%")
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

  subtitle <- paste("Last:", my_xts[length(my_xts), 1], paste0("(",index(my_xts)[length(my_xts)], ")"),
                    "Prev:", my_xts[length(my_xts) - 1, 1],
                    "Chg:", format((coredata(my_xts[length(my_xts), 1]) - coredata(my_xts[length(my_xts) - 1, 1])), nsmall = 2)
  )

  ggplot(my_xts, aes(x = Index, y = my_xts[,1])) + geom_line()  + geom_point(color = "blue") + labs(title = title, subtitle = subtitle, y = "Index", x = "Date")
}
