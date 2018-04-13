library(dplyr)
library(ggplot2)
library(Rblpapi)
library(MacrobondAPI)

#' Plot cycles chart using Bloomberg data
#'
#' @param ticker BBG ticker of securty
#' @param field  BBG field code of the security
#' @param cycles   dataframe containing cycles with columns `from` and `to` as Dates
#' @param start_date  required start date if any
#' @param title   title of the chart
#' @param xlab   xlab for the chart
#' @param ylab   ylab for the chart
#'
#' @examples plot_cycles("MXEM Index", "EQY_DVD_YLD_12M", cycles, 
#              title = "Dividend Yield, MSCI EMU, CEPR Business Cycles (Trough to Peak)",
#              ylab = "Dividend Yield (%)")
plot_cycles_bbg <- function(ticker, field, cycles, start_date = NA, title = "", xlab = "Proportion of cycle", ylab="") {
  # create description for cycles if not present
  if (is.null(cycles$des)) {
    cycles <- data.frame(from = as.Date(c("1993-07-01", "2009-04-01", "2013-01-01")),
                              to = c(as.Date(c("2008-04-1", "2011-10-01", "2018-12-31"))))  %>%
      mutate(des = reorder(factor(paste(format(from, "%b %y"), format(to, "%b %y"), sep = " to ")),from))
  }

  # start date as given or earliest available
  start_date <- as.Date(ifelse(is.na(start_date), min(cycles$from), start_date))

  # Download data from BBG
  df <- getData(ticker, fields = field, start_date = start_date)
  
  #add empty columns
  df <- df %>% mutate(from = as.Date(NA), to = as.Date(NA), 
                      des = factor(NA, levels = cycles$des[order(cycles$from)]))
  
  # find data that hit the matched
  for (i in 1:nrow(df)) {
    matched <- which((df$date[i] >= cycles$from) & (df$date[i] <= cycles$to))
    if (length(matched) > 0) {
      df[i,c('from','to','des')] <- cycles[matched,]
    }
  }
  
  # Filter out overlaps and calculate proportion
  df <- df %>% filter(!is.na(des)) %>%
    group_by(des) %>%
    mutate(x = as.numeric(date - from) / as.numeric(to - from))
  
  df %>%
    ggplot(aes_string(x="x", y=field, col = "des")) +
    geom_line(size = 1.5) +
    labs(title = title,
         x = xlab, y = ylab)  
}

#' Format gg_plots according to our specifications
#'
#' @param gg_obj ggplot object
#' @param format_name name of format
#'
#' @return ggplot with specificed formats
#'
format_plot <- function(gg_obj, format_name) {
  if (format_name == "eqd_strat") {
    gg_obj +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5, size = 13)) +
      theme(legend.position = "bottom", legend.title=element_blank(),
            legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
            legend.key.height=unit(0, "cm"),) 
  }
  else {
    stop("Unrecognized format")
  }
}
