#' @import dplyr
#' @import ggplot2
#' @importFrom purrr pmap
#' @importFrom lubridate origin

NULL

#' Plot cycles chart
#'
#' @param df dataframe containing date and value
#' @param cycles_df   dataframe containing cycles with columns `from` and `to` as Dates
#' @param title   optional title of the chart
#' @param xlab   optional xlab for the chart
#' @param ylab   optional ylab for the chart
#'
#' @export
#'
#' @examples
#' data(ez_biz_cycles)
#' data(pe_ratio_mxem)
#' plot_cycles(pe_ratio_mxem, ez_biz_cycles)

plot_cycles <- function(df, cycles_df, title ="", xlab = "Proportion of cycle", ylab = NULL) {
  col_to_plot <- names(df)[names(df) != "date"]

  if (length(col_to_plot) > 1) {
    col_to_plot <- col_to_plot[1]
    warning(paste("multiple columns in df found, plotting", col_to_plot))
  }

  # Extract out prices for each cycle
  cycle_prices <- purrr::pmap(cycles_df,
                       function(from, to) {
                         cycle_label <- paste(as.Date(from, origin = lubridate::origin), "to", as.Date(to, origin = lubridate::origin))
                         dplyr::filter(df, .data$date >= from & .data$date <= to)  %>%
                           dplyr::mutate(cycle_n = cycle_label)
                       }) %>%
    bind_rows()

  # Calculate proportion of cycle for each date
  cycle_prop <- cycle_prices %>%
    group_by(.data$cycle_n) %>%
    mutate(x = as.numeric(.data$date - min(.data$date)) / as.numeric(max(.data$date) - min(.data$date)))

  # Plot cycle chart
  g <- cycle_prop %>%
    ggplot(aes(x = .data$x, colour = .data$cycle_n)) +
    geom_line(aes_string(y = col_to_plot), size = 1.5)

  # Add ylab if necessary
  if (is.null(ylab)) {
    g + labs(title = title, x = xlab)
  } else {
    g + labs(title = title, x = xlab, y = ylab)
  }
}

#############################################
#
#     GGPLOT THEMES AND COLOR SCHEMES
#
#############################################

#' Charting theme for ggplot
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(pe_ratio_mxem)
#' ggplot(pe_ratio_mxem, aes(x = date, y = PE_RATIO)) + geom_line() + theme_strat()
theme_strat <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 13),
          legend.position = "bottom", legend.title=element_blank(),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
          legend.key.height=unit(0, "cm"),
          line = element_line(size= 1.2))
}

#' Colour schemes for ggplot
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(pe_ratio_mxem)
#' df <-  dplyr::mutate(pe_ratio_mxem, date_range = cut(date, 5))
#' ggplot(df, aes(x = date, y = PE_RATIO, color = date_range)) + geom_line() + scale_color_strat()
scale_color_strat <- function() {
  col_strat <-c("#BB272E", "#2769BB", "#A4BB27", "#BB7927", "#3E27BB", "#27BB5B", "#9627BB", "#000000")
  scale_color_manual(values = col_strat)
}




#' ez_biz_cycles
#' @name ez_biz_cycles
#' @docType data
#' @details Business cycles in the Eurozone
NULL

#' pe_ratio_mxem
#' @name pe_ratio_mxem
#' @docType data
#' @details Time series PE ratio of MSCI EMU Index
NULL
