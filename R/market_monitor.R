library(Rblpapi)

blpConnect()

getData <- function(Ticker){
  bdh(Ticker, c("PX_LAST"), start.date = Sys.Date()-365)
}

ggTS <- function(Ticker, Title = Ticker){
  data <- getData(Ticker = Ticker)

  ggplot2::ggplot(data, ggplot2::aes(x = date, y = PX_LAST)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(colour = "darkgreen") +
    ggplot2::labs(title = Ticker, y = "Price", x = "Date")

}

ggTS("GBPUSD Curncy")
ggTS("GBPEUR Curncy")

