#required_libraries <- c("Rblpapi", "pbapply", "stringr", "tidyverse", "ggplot2")
#install.packages(required_libraries)

#' Merge a list of dataframes with a single data column each into a single dataframe
#'
#' @param df_list list of dataframes
#' @param header_names used when df_list items are unnamed
#'
#' @return dataframe with column names as `header_names` or name of list elements of `df_list`
#' @export
#'
#' @examples
#'   bbg_series <- c("SPX Index", "UKX Index"); field <- "PX_LAST"
#'   data_bbg <- pblapply(1:length(bbg_series),
#'     function(x) { bdh(bbg_series[x], field, start_date = as.Date("2018-01-01"), end_date = as.Date("2018-10-10")) })
#'   df <- merge_from_list(data_bbg)
merge_from_list <- function(df_list, header_names = NULL) {
  if (is.null(header_names) && is.null(names(df_list))) stop("either header_names must be provided or list must be named")
  df <- suppressWarnings( # Warning is suppressed as column names will be duplicated
    reduce(df_list, function(x,y) {merge(x, y, by ="date", all = T)})
  )
  if (is.null(header_names))
    header_names <- names(df_list)
  colnames(df) <- c("date", header_names)
  df
}

#' Batch download from Bloomberg
#'
#' @param series dataframe or vector. If dataframe, `ticker` column is necessary, `name` is optional for naming the results, otherwise `ticker` is used
#' @param field Bbg field to be downloaded, defaults to `PX_LAST`
#' @param start_date start date of data, defaults to start of 1994 (note that `bdh()` uses `start.date`)
#' @param end_date end date, if omitted, download to most recent available
#' @param options options to be passed to bdh()
#'
#' @return dataframe of historical data
#' @export
#'
#' @examples
#'   futures_price <- bdh_batch(c("RX1 Comdty", "TY1 Comdty"), "CONTRACT_VALUE", start_date = as.Date("2018-01-01"))
#'
#'   df <- data.frame(ticker = c("RX1 Comdty", "TY1 Comdty"), name = c("Bunds", "Treasury"))
#'   futures_price <- bdh_batch(df, "CONTRACT_VALUE", start_date = as.Date("2018-01-01"), end_date = as.Date("2018-06-06"))
bdh_batch <- function(series, field = "PX_LAST", start_date = as.Date("1994-01-01"), end_date = NULL, options = NULL) {
  if (is.null(options))
    options <- c("periodicitySelection" = "DAILY",
                 "nonTradingDayFillOption" = "NON_TRADING_WEEKDAYS",
                 "nonTradingDayFillMethod" = "PREVIOUS_VALUE")

  ## ADDITIONAL OPTIONS FOR REFERENCE
  # option.fields <- c("periodicitySelection", "nonTradingDayFillOption",
  #                    "nonTradingDayFillMethod", "periodicityAdjustment",
  #                    "adjustmentFollowDPDF", "currency")
  #
  # option.values <- c("DAILY", "NON_TRADING_WEEKDAYS", "NIL_VALUE",
  #                    "CALENDAR", "TRUE", "USD")

  # If series is data.frame, match name to ticker
  if (class(series) == 'data.frame') {
    if (is.null(series$ticker)) stop("Input series is a dataframe, column 'ticker' is missing")

    bbg_series <- series$ticker
    nm <- if (is.null(series$name)) series$ticker else series$name
  }

  # If series is just a character vector, search for name of vector, otherwise just use ticker as header
  else {
    bbg_series <- series
    nm <- if (is.null(names(series))) series else names(series)
  }

  data_bbg <- pblapply(1:length(bbg_series),
                       function(x) {
                         bdh(bbg_series[x], field, start.date = start_date, end.date = end_date, options = options)
                       })
  df <- merge_from_list(data_bbg, header_names = nm)
  rownames(df) <- df$date
  df
}

#' Remove the date column from a dataframe and store it into rownames (used for preprocessing of dataframes for mass calculations e.g. returns)
#'
#' @param df dataframe with date (without date also works, it just does nothing)
#'
#' @return dataframe without the date columns. date will be stored in rownames
#' @export
#'
remove_date <- function(df) {
  if ("date" %in% names(df)) {
    df <- as.data.frame(df)
    rownames(df) <- as.Date(df$date)
    df <- df %>% select(-date)
  }
  df
}

#' Calculate daily total return in a dataframe by summing across assets
#'
#' @param df dataframe of returns, `date` column` is optional
#' @param return_date if `TRUE`, include date in the return dataframe
#' @param na.rm if `TRUE`, rowsum will be NA when there are any NAs within the row
#'
#' @return dataframe containing total return across rows
#' @export
#'
#' @examples
#' df <- data.frame(spx = c(0.01, -0.02, -0.05), ukx = c(0.014, 0.08, -0.04))
#' compute_total_ret(df)
compute_total_ret <- function(df, return_date = FALSE, na.rm = FALSE) {
  df$ret <- if (!is.null(df$date)) rowSums(select(df, -date), na.rm = na.rm) else rowSums(df, na.rm = na.rm)

  if (return_date)
    df <- if (!is.null(df$date)) select(df, date, ret) else select(df, ret)
  df
}

#' Calculate individual daily returns of a dataframe containing multiple assets
#'
#' @param df dataframe containing asset prices, `date` column is optional
#'
#' @return dataframe containing returns
#' @export
#'
#' @examples
#' df <- data.frame(spx = c(2850, 2820, 2890), ukx = c(7004, 7010, 7080))
#' compute_returns(df)
compute_returns <- function(df) {
  has_date <- !is.null(df$date)
  if (has_date) df <- remove_date(df)
  # Compute returns
  df <- df / mutate_all(df, funs(lag)) -1

  if (has_date) cbind(date = row.names(df), df)
  else df
}

#' Calculate active risk given return of strategies and weight of the strategies
#'
#' @param unwt_ret_w_date dataframe of returns of strategies
#' @param curr_wt dataframe (one row) or vector of weights, df must have colnames and vector must have names corresponding to colname of returns
#' @param start_date start_date for calc of active risk
#' @param end_date end_date
#' @param annualize_factor factor to multiply by to convert to annual. 250 for daily data (default) and 12 for monthly data
#'
#' @return list containing `cov_matrix`, `port_sd` (portfolio std dev), `active_risk`, `marginal_risk`
#' @export
#'
#' @examples
#' unwt_ret_w_date <- data.frame(date = as.Date(c("2018-01-02", "2018-01-02", "2018-01-04", "2018-01-05")),
#'                               long_spx = c(0.015, 0.021, -0.03, 0.01),
#'                               long_ukx = c(-0.005, 0.03, -0.01, -0.04))
#' curr_wt <- data.frame(long_spx = 0.01, )
#' results <- calc_active_risk(unwt_ret_w_date, curr_wt)
#' results$active_risk
calc_active_risk <- function(unwt_ret_w_date, curr_wt, start_date = NULL, end_date = NULL, annualize_factor = 250) {
  # If weights provided as data.frame, convert to vector for manipulation purposes
  if (class(curr_wt) == "data.frame") {
    wt <- remove_date(curr_wt)
    nam <- colnames(curr_wt)
    curr_wt <- as.numeric(curr_wt)
    names(curr_wt) <- nam
  }

  if (!is.null(start_date) && !is.na(start_date)) {
    if (is.null(end_date) || is.na(end_date)) {
      end_date <- today()
      warning("No end_date provided, today is used")
    }
    unwt_ret_w_date <- unwt_ret_w_date %>% filter(date > start_date & date <= end_date)
  }
  unwt_ret_fn <- unwt_ret_w_date %>% remove_date()

  # Filter only those strategies with weights
  unwt_ret_fn <- unwt_ret_fn[,names(curr_wt)]

  # Compute active risk
  cov_matrix <- cov(unwt_ret_fn, use = "complete.obs") * annualize_factor # annualized

  numerator <- as.numeric(t(curr_wt) %*% cov_matrix)
  names(numerator) <- names(unwt_ret_fn)

  port_var <- as.numeric(t(curr_wt) %*% cov_matrix %*% curr_wt)
  port_sd <- sqrt(port_var)

  marginal_risk <- numerator / port_sd
  active_risk <- marginal_risk * curr_wt

  names(marginal_risk) <- names(curr_wt)
  list(cov_matrix = cov_matrix, port_sd = port_sd, active_risk = active_risk, marginal_risk = marginal_risk)
}

# Function for
#' Calculate correlation between returns
#'
#' @param unwt_ret_df dataframe containing timeseries returns for each strategy
#' @param start_date start_date of the period for correlation calculation
#' @param end_date
#' @param period_name optional period_name to attach to the returned correlations
#'
#' @return long form dataframe (see tidyr::gather) containing correlation between returns, and `period_name`` if provided
#' @export
#'
#' @examples
#' unwt_ret <- data.frame(date = as.Date(c("2018-01-02", "2018-01-02", "2018-01-04", "2018-01-05")),
#'                               long_spx = c(0.015, 0.021, -0.03, 0.01),
#'                               long_ukx = c(-0.005, 0.03, -0.01, -0.04),
#'                               long_hsi = c(0.023, 0.001, -0.005, 0.008))
#' calc_cor(unwt_ret)
calc_cor <- function(unwt_ret_df, start_date = NA, end_date = NA, period_name = NULL) {
  clear_diag <- function(x) {
    diag(x) <- NA
    x
  }

  if (!is.na(start_date)) {
    if (is.na(end_date))
      end_date <- today()
    unwt_ret_df <- unwt_ret_df %>% filter(date > start_date & date <= end_date)
  }

  df <- cor(unwt_ret_df %>% remove_date, use = "pairwise.complete.obs") %>%
    clear_diag %>%
    as.data.frame %>%
    rownames_to_column("strat1") %>%
    gather(strat2, corr, -strat1)

  if (!is.null(period_name)) {
    df <- df %>% mutate(period = period_name)
  }

  df
}

#' Plotting correlation heatmaps based on correlation of assets
#'
#' @param cor_df long form dataframe (see tidyr::gather), containing columns `strat1`, `strat2`, `corr`
#' @param title optional title of chart
#'
#' @return None
#' @export
#'
#' @examples
#' unwt_ret <- data.frame(date = as.Date(c("2018-01-02", "2018-01-02", "2018-01-04", "2018-01-05")),
#'                               long_spx = c(0.015, 0.021, -0.03, 0.01),
#'                               long_ukx = c(-0.005, 0.03, -0.01, -0.04),
#'                               long_hsi = c(0.023, 0.001, -0.005, 0.008))
#' cor_df <- calc_cor(unwt_ret)
#' plot_cor(cor_df)
plot_cor <- function(cor_df, title = NULL) {
  print(
    cor_df %>%
      ggplot(aes(x = strat1, y = strat2)) +
      geom_tile(aes(fill = corr)) +
      geom_text(aes(label=formatC(corr, digits = 2, format = "f"))) +
      scale_fill_gradient2(low = "red", high = "green", mid ="white", midpoint = 0, limits = c(-1, 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank()) +
      labs(title = title)
  )
}

#' Helper function to sort categories for display in ggplot, for ensuring that column charts are ordered by magnitude
#'
#' @param df_gathered long form dataframe (see tidyr::gather) for plotting
#' @param group_by_column name of column to group by
#' @param sort_by name of column to sort by
#' @param filter_scenario filters data to the required scenario, defaults to `Last3M`
#'
#' @return dataframe but with category levels sorted
#' @export
#'
#' @examples
#' input_text <- "period asset_class active_risk\nLast3M Equity 1.256309e-05\nLast3M Fixed_Income 1.389163e-03\nLast3M FX 1.531547e-04\nLast3M Others 2.138171e-05\nGFCStress Equity 2.083595e-04\nGFCStress Fixed_Income 4.309154e-03\nGFCStress FX 9.310046e-04\nGFCStress Others 2.353732e-04\nTaperTantrum Equity 2.892491e-05\nTaperTantrum Fixed_Income 2.097302e-03\nTaperTantrum FX 1.601432e-04\nTaperTantrum Others 6.576230e-05"
#' input_table <- read.table(text=input_text, header = 1)
#'
#' input_table %>%
#'   sort_gg("asset_class", "active_risk") %>%
#'   ggplot(aes(x = asset_class, y = active_risk * 10000)) +
#'   geom_col() +
#'   facet_wrap(~period, ncol = 3)
sort_gg <- function(df_gathered, group_by_column, sort_by, filter_scenario = "Last3M") {
  ordered_column <- df_gathered %>%
    filter(period == filter_scenario) %>%
    group_by_(group_by_column) %>%
    summarise(stat = sum(!!sym(sort_by))) %>%
    arrange(desc(stat)) %>%
    .[[group_by_column]] %>%
    as.character %>%
    rev

  df_gathered[[group_by_column]] <- factor(df_gathered[[group_by_column]], levels = ordered_column)
  df_gathered
}
