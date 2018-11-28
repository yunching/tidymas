library(lubridate)
library(tidyverse)

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

bdh_weekday <- function(security, field = "PX_LAST", start_date = as.Date("1994-01-01"), end_date = NULL, options = NULL) {
  if (is.null(options))
    options <- c("periodicitySelection" = "DAILY",
                 "nonTradingDayFillOption" = "NON_TRADING_WEEKDAYS",
                 "nonTradingDayFillMethod" = "PREVIOUS_VALUE")

  bdh(security, field, start.date = start_date, end.date = end_date, options = options)
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
  if (any(class(series) == 'data.frame')) {
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

gen_weekdays <- function(start_date, end_date) {
  if (is.na(end_date)) end_date <- today()
  if (is.na(start_date)) start_date <- today()

  dt <- seq.Date(start_date, end_date, 1)
  dt[!weekdays(dt) %in% c("Saturday", "Sunday")]
}

get_tickers <- function(asset_class, roll_differencing = TRUE) {
  if (asset_class == "govt") {
    output <- read.csv(system.file("data2", "tickers_map", "tickers_govt.csv", package="tidymas"), stringsAsFactors = FALSE)
  }
  else if (asset_class == "fut") {
    output <- read.csv(system.file("data2", "tickers_map", "tickers_futures.csv", package="tidymas"), stringsAsFactors = FALSE) %>%
      mutate(asset_class = "fut")

    # Roll differencing, see ‘DOCS #2072138 <GO> on Bloomberg
    if (roll_differencing)
      output <- output %>% mutate(ticker = str_replace(ticker, "(?i) comdty", " B:00_0_D Comdty"))
  }
  else if (asset_class == "equity") {
    output <- read.csv(system.file("data2", "tickers_map", "tickers_equity.csv", package="tidymas"), stringsAsFactors = FALSE) %>%
      mutate(asset_class = "equity")
  }
  else if (asset_class == "cds") {
    output <- read.csv(system.file("data2", "tickers_map", "tickers_cds.csv", package="tidymas"), stringsAsFactors = FALSE) %>%
      mutate(asset_class = "cds")
  }
  else if (asset_class == "fx") {
    output <- read.csv(system.file("data2", "tickers_map", "tickers_funding.csv", package="tidymas"), stringsAsFactors = FALSE)
  }
  else {
    stop("Invalid asset_class, only can handle govt, fut, equity, cds, fx")
  }

  output
}

get_and_check_tickers <- function(curr_asset_class, instruments_df, type = "price") {
  # Type is price, duration
  # Asset class work for bonds, equity, cds, fut (does not support fx)
  if (!curr_asset_class %in% c("govt", "equity", "cds", "fut")) stop("Asset class not supported, only govt, equity, cds, fut allowed")
  if (!type %in% c("price", "duration")) stop("Type not supported, only price and duration allowed")

  sec_tickers <- get_tickers(curr_asset_class)


  # Match tickers to identifier
  if (curr_asset_class == "cds") {
    sec_df <- instruments_df %>%
      filter(asset_class == "cds") %>%
      left_join(sec_tickers, by = c("asset_class", "identifier")) %>%
      mutate(name = identifier)
    if (type == "price") {
      sec_df <- mutate(sec_df, ticker = ticker_return)
    }
    else if (type == "duration") {
      sec_df <- mutate(sec_df, ticker = ticker_duration)
    }
  }
  else {
    if (curr_asset_class == "govt") curr_asset_class <- c("govt", "ilb")

    sec_df <- instruments_df %>%
      filter(asset_class %in% curr_asset_class) %>%
      left_join(sec_tickers, by = c("asset_class", "identifier")) %>%
      mutate(name = identifier)
  }

  # Check valid identifier for bonds ie bonds have corresponding tickers
  if (any(is.na(sec_df$ticker))) {
    invalid_sec <- sec_df %>%
      filter(is.na(ticker)) %>%
      mutate(error_msg = paste(asset_class, identifier, sep = "/"))
    stop(paste("Unable to find ticker for the following bonds:",paste(invalid_sec$error_msg, collapse = ",")))
  }

  # Warning for ignored instrument classes
  ignored_sec <- instruments_df %>%
    filter(!asset_class %in% curr_asset_class) %>%
    mutate(warning_msg = paste(asset_class,identifier,sep="/"))

  if (nrow(ignored_sec) > 0) {
    warning(paste("Instruments ignored in",curr_asset_class, paste(unique(ignored_sec$warning_msg), collapse = ",")))
  }

  reduced_sec_df <- sec_df %>%
    group_by(name, ticker) %>%
    summarise()

  if (nrow(reduced_sec_df) == 0) {
    stop(paste("No tickers detected for asset class:", curr_asset_class))
  }
  reduced_sec_df
}

build_strategies <- function(input_file, start_date = as.Date("2000-01-01"), end_date = today()) {
  # Read in strategies
  strategies <- read.csv(input_file, stringsAsFactors = FALSE) %>%
    mutate(open_date = ymd(open_date), close_date = ymd(close_date),
           strategy = paste(strategy, owner, sep = ":::"))

  # Check asset classes
  valid_asset_class <- c("govt", "ilb", "fx", "equity", "cds", "fut")
  asset_classes <- strategies$asset_class %>% unique
  if (mean(asset_classes %in% valid_asset_class) < 1)
    stop(paste("Invalid asset class found in", input_file, ":", paste(asset_classes[! asset_classes %in% valid_asset_class], collapse = ",")))

  # Check if differing size_types for same instrument in the same strategy
  unique_sizes_for_instrument <- strategies %>%
    group_by(strategy, identifier, size_type) %>%
    summarise() %>%
    group_by(strategy, identifier) %>%
    summarise(count = n())

  if (any(unique_sizes_for_instrument$count > 1)) {
    invalid_strategies <- subset(unique_sizes_for_instrument, count > 1) %>%
      mutate(error_msg = paste(strategy, instrument, sep = "/"))
    stop(paste("Multiple size types detected for the same instrument in the same strategy:", paste(invalid_strategies$error_msg, collapse = ", ")))
  }

  govt_tickers <- get_tickers("govt")
  fx_tickers <- get_tickers("fx")
  eq_tickers <- get_tickers("equity")
  cds_tickers <- get_tickers("cds")
  fut_tickers <- get_tickers("fut")

  non_fxcds_tickers <- rbind(
    govt_tickers,
    eq_tickers,
    fut_tickers
  )

  # Check tickers for FX
  fx_errors <- strategies %>%
    filter(asset_class == "fx") %>%
    mutate(left_curr = substr(identifier, 1, 3),
           right_curr = substr(identifier, 4, 6),
           error = (!((left_curr %in% fx_tickers$currency) & (right_curr %in% fx_tickers$currency))) | (nchar(identifier) != 6)) %>%
    filter(error)
  if (nrow(fx_errors) > 0)
    stop(paste("Invalid fx identifiers:", paste(fx_errors$identifier, collapse = ", ")))

  # Check tickers for CDS
  cds_errors <- strategies[10:20,] %>%
    filter(asset_class == "cds") %>%
    left_join(cds_tickers, by = c("asset_class", "identifier")) %>%
    filter(is.na(ticker_return))

  if (nrow(cds_errors) > 0) {
    stop(paste("Invalid cds identifiers:", paste(cds_errors$identifier, collapse = ",")))
  }

  # Check tickers exist for other instruments
  strat_inst <- strategies %>%
    filter(asset_class %in% c("govt", "ilb", "fut", "equity")) %>%
    group_by(identifier, asset_class) %>%
    summarise %>%
    left_join(non_fxcds_tickers, by = c("asset_class", "identifier"))

  if (any(is.na(strat_inst$ticker))) {
    not_found <- strat_inst %>% filter(is.na(ticker)) %>%
      mutate(error_msg = paste(asset_class, identifier, sep = "/"))
    stop(paste("Identifier not found: ", paste(not_found$error_msg, collapse = ",")))
  }

  # Summarise strategies (for use in size conversions later)
  strat_summ <- strategies %>%
    group_by(strategy, identifier, asset_class, size_type) %>%
    summarise()

  # Calculate sizes
  all_actual_size <- NULL
  all_sim_size <- NULL
  ## Iterate through each unique strategy
  for (i in unique(strategies$strategy)) {
    # Filter out current strategy
    curr_strat <- strategies %>% filter(strategy == i)
    inst_list <- curr_strat$identifier %>% unique
    actual_size <- data.frame(date = gen_weekdays(start_date, end_date))

    # Iterate through all identifiers required for this strategy
    for (curr_inst in inst_list) {
      curr_inst_df <- curr_strat %>% filter(identifier == curr_inst)

      #curr_inst_size <- data.frame(date = dt, size_type = curr_inst_df$size_type[1], strategy = curr_strat$strategy[1], identifier = curr_inst, stringsAsFactors = FALSE)
      actual_size[[curr_inst]] <- 0

      # Set weight as actual first
      for (j in 1:nrow(curr_inst_df)) {
        # Strat start and end
        strat_start <- if (is.na(curr_inst_df$open_date[j])) as.Date(today()-days(1)) else curr_inst_df$open_date[j]
        strat_end <- if (is.na(curr_inst_df$close_date[j])) as.Date(today()) else curr_inst_df$close_date[j]

        actual_size[[curr_inst]] <- actual_size[[curr_inst]] +
          if_else(actual_size$date <= strat_end & actual_size$date > strat_start,
                  curr_inst_df$size[j], 0)
      }
    }
    sim_size <- actual_size %>%
      mutate(has_position = sign(rowSums(abs(.[-1])))) %>%  # Check if there's any position at the time
      gather(asset, size, -date, -has_position) %>%  # convert into gathered dataframe
      mutate(size = ifelse(has_position, size, NA)) %>%   # if does not have any position, replace with NA
      spread(asset, size) %>%  # Convert back into original layout
      select(-has_position) %>%
      fill(-date, .direction = "up")  # Fill upwards to times without position, to calculate returns for simulation and correlations

    all_actual_size[[i]] <- actual_size
    all_sim_size[[i]] <- sim_size
  }

  list(summary = strat_summ, actual = all_actual_size, sim = all_sim_size)
}

get_dur_bonds_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today(), fill = TRUE) {
  reduced_sec_df <- get_and_check_tickers("govt", instruments_df)

  print("Downloading bond duration from bbg...")
  dur_df <- bdh_batch(reduced_sec_df, "MODIFIED_DURATION", start_date = start_date, end_date = end_date)

  # Some instruments have very short duration history e.g. ILBs, hence we just assume all previous to be the first duration.
  if (fill)
    dur_df <- tidyr::fill(dur_df, everything(), .direction = "up")

  dur_df
}

get_dur_fut_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today(), fill = TRUE) {
  reduced_sec_df <- get_and_check_tickers("fut", instruments_df)

  # Download data from Bloomberg
  print("Downloading futures duration from bbg...")
  dur_df <- bdh_batch(reduced_sec_df, "FUT_EQV_DUR_NOTL", start_date = start_date, end_date = end_date)

  # Download duration where only most recent is available e.g. Euribors
  all_nas <- dur_df %>% gather(identifier, duration, -date) %>%
    group_by(identifier) %>%
    summarise(na = mean(is.na(duration))) %>%
    filter(na == 1) %>%
    left_join(reduced_sec_df, by = c("identifier"="name"))

  recent_dur <- bdp(all_nas$ticker, "FUT_EQV_DUR_NOTL")
  recent_dur$identifier <- all_nas$identifier

  for (i in 1:nrow(recent_dur)) {
    dur_df[[recent_dur$identifier[i]]] <- recent_dur[i, 1]
  }

  # Some instruments have very short duration history e.g. ILBs, hence we just assume all previous to be the first duration.
  if (fill)
    dur_df <- tidyr::fill(dur_df, everything(), .direction = "up")

  dur_df
}

get_dur_cds_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today(), fill = TRUE) {
  reduced_sec_df <- get_and_check_tickers("cds", instruments_df, "duration")

  # Download data from Bloomberg
  cds_dv01 <- bdp(reduced_sec_df$ticker, "SW_EQV_BPV")

  cds_dur <- abs(cds_dv01 * 0.001)
  cds_dur$identifier <- reduced_sec_df$name

  dur_df <- data.frame(date = gen_weekdays(start_date, end_date))

  # Note that Bloomberg can pull out historical DV01, however, it requires using bdp() with overwrites.
  # This can significantly reduce performance of the function. For CDX, the DV01 exposure fluctuates within +-10%, hence it would be
  # reasonable to just use the latest as reference
  for (i in 1:nrow(cds_dur)) {
    dur_df[[cds_dur$identifier[i]]] <- cds_dur[i, 1]
  }

  dur_df
}

get_dur_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today(), fill = TRUE) {
  sec_df <- instruments_df %>%
    group_by(identifier, asset_class) %>%
    summarise()

  bonds <- filter(sec_df, asset_class %in% c("govt", "ilb"))
  fut <- filter(sec_df, asset_class == "fut")
  cds <- filter(sec_df, asset_class == "cds")

  dur <- list()
  if (nrow(bonds) > 0)
    dur$bonds <- get_dur_bonds_bbg(bonds, start_date = start_date, end_date = end_date, fill = fill)

  if (nrow(fut) > 0)
    dur$fut <- get_dur_fut_bbg(fut, start_date = start_date, end_date = end_date, fill = fill)

  if (nrow(cds) > 0)
    dur$cds <- get_dur_cds_bbg(cds, start_date = start_date, end_date = end_date, fill = fill)

  dur_all <- reduce(dur, function(x, y) {
    if (is.null(x))
      y
    else if (is.null(y))
      x
    else
      full_join(x, y, by = "date")
  })

  dur_all
}

convert_dur_size <- function(strat_df, strat_id_sizetype, duration_df, convert_to_decimal = TRUE) {
  # Check if there are any missing duration
  missing_duration <- filter(strat_id_sizetype, size_type == "months" & (! identifier %in% names(duration_df))) %>%
    .$identifier %>%
    unique
  if (length(missing_duration) > 0) stop(paste("duration missing:", paste(missing_duration, collapse = ",")))

  # Check for missing dates
  if (any(!strat_df[[1]]$date %in% duration_df$date)) {
    missing_dates <- subset(strat_df[[1]], ! date %in% duration_df$date) %>%
      .$date
    stop("Duration missing data for dates: ", paste(missing_dates, collapse = ","))
  }

  # Convert from months weighted to %
  months_strategies <- strat_id_sizetype %>%
    filter(size_type == "months")

  ## Extract only dates of duration from the strategies
  req_dur <- duration_df %>% filter(date %in% strat_df[[1]]$date)

  ## portfolios are all lists inside the strategies_list except summary

  for (i in 1:nrow(months_strategies)) {
    curr_strat <- months_strategies$strategy[i]
    curr_inst <- months_strategies$identifier[i]

    strat_df[[curr_strat]][[curr_inst]] <- strat_df[[curr_strat]][[curr_inst]] / (req_dur[[curr_inst]] * 12) * 100
  }

  # Convert to decimal for easier return calculation
  if (convert_to_decimal) {
    strat_df <- lapply(strat_df, function(x) mutate_at(x, vars(-date), function(y) y/100))
  }

  strat_df
}

get_ret_bonds_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  reduced_sec_df <- get_and_check_tickers("govt", instruments_df)

  print("Downloading bond prices from bbg...")
  price_df <- bdh_batch(reduced_sec_df, "PX_LAST", start_date = start_date, end_date = end_date)

  calc_returns(price_df)
}

get_ret_fx_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  reduced_fx <- instruments_df %>% filter(asset_class == "fx") %>%
    mutate(left_curncy = substr(identifier, 1, 3),
           right_curncy = substr(identifier, 4, 6),
           ticker = paste(toupper(identifier), "Curncy"))

  # Calculate price return
  req_fx <- reduced_fx %>%
    group_by(identifier, ticker) %>%
    summarise() %>%
    mutate(name = identifier)

  print("Downloading fx prices...")
  fx_index <- bdh_batch(req_fx, start_date = start_date, end_date = end_date)

  fx_price_ret <- calc_returns(fx_index)

  # Download funding rates from Bloomberg
  req_curncy <- unique(c(reduced_fx$left_curncy, reduced_fx$right_curncy))

  req_fx_depo <- get_tickers("fx") %>%
    filter(currency %in% req_curncy) %>%
    mutate(name = currency)

  print("Downloading depo rates...")
  fx_funding_rates <- bdh_batch(req_fx_depo, start_date = start_date, end_date = end_date)

  # Funding based on actual/360, hence calculate number of days between dates (to account for weekends)
  day_count <- as.numeric(fx_funding_rates$date - lag(fx_funding_rates$date))

  # Calculate funding return for each day
  fx_funding_ret <- fx_funding_rates %>% remove_date %>%
    .[]/360 * day_count / 100  # rate / (days in year) * day_count / (adjustment for %)

  fx_funding_ret <- fx_funding_ret %>%
    mutate_all(funs(lag))   # Lag one day

  # Earn the left funding and pay the right funding for the currency pair
  left_funding <- fx_funding_ret[,substr(names(fx_price_ret)[-1], 1, 3)]
  right_funding <- fx_funding_ret[,substr(names(fx_price_ret)[-1], 4, 6)]
  total_funding <- left_funding - right_funding  # Earn left pair funding, pay right pair

  # Fill NAs for dates without funding (of either pair), may lose return accuracy but allows for calculation in the earlier bits of history
  total_funding[is.na(total_funding)] <- 0


  # Total return is price return + funding return
  fx_total_ret <- remove_date(fx_price_ret) + total_funding

  fx_total_ret <- cbind(data.frame(date = as.Date(rownames(fx_total_ret))), fx_total_ret)

  fx_total_ret
}

get_ret_equity_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  reduced_sec_df <- get_and_check_tickers("equity", instruments_df)

  print("Downloading equity prices from bbg...")
  price_df <- bdh_batch(reduced_sec_df, "PX_LAST", start_date = start_date, end_date = end_date)

  calc_returns(price_df)
}

get_ret_cds_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  reduced_sec_df <- get_and_check_tickers("cds", instruments_df, type = "price")

  print("Downloading cds prices from bbg...")
  price_df <- bdh_batch(reduced_sec_df, "PX_LAST", start_date = start_date, end_date = end_date)

  calc_returns(price_df)
}

get_ret_fut_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  reduced_sec_df <- get_and_check_tickers("fut", instruments_df)

  print("Downloading futures prices from bbg...")
  price_df <- bdh_batch(reduced_sec_df, "CONTRACT_VALUE", start_date = start_date, end_date = end_date)

  calc_returns(price_df)
}

get_ret_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  sec_df <- instruments_df %>%
    group_by(identifier, asset_class) %>%
    summarise()

  bonds <- filter(sec_df, asset_class %in% c("govt", "ilb"))
  fut <- filter(sec_df, asset_class == "fut")
  cds <- filter(sec_df, asset_class == "cds")
  fx <- filter(sec_df, asset_class == "fx")
  equity <- filter(sec_df, asset_class == "equity")

  ret <- list()
  if (nrow(bonds) > 0)
    ret$bonds <- get_ret_bonds_bbg(bonds, start_date = start_date, end_date = end_date)

  if (nrow(fut) > 0)
    ret$fut <- get_ret_fut_bbg(fut, start_date = start_date, end_date = end_date)

  if (nrow(cds) > 0)
    ret$cds <- get_ret_cds_bbg(cds, start_date = start_date, end_date = end_date)

  if (nrow(fx) > 0)
    ret$fx <- get_ret_fx_bbg(fx, start_date = start_date, end_date = end_date)

  if (nrow(equity) > 0)
    ret$equity <- get_ret_equity_bbg(equity, start_date = start_date, end_date = end_date)

  # Combine all into a single dataframe
  ret_all <- reduce(ret, function(x, y) {
    if (is.null(x))
      y
    else if (is.null(y))
      x
    else
      full_join(x, y, by = "date")
  })

  ret_all
}

calc_strat_wt_return <- function(strat_df, asset_returns) {
  cbind(date = strat_df[[1]]$date,
        setNames( ## setNames to original as map changes the : to .
          as.data.frame( ## Convert product to dataframe
            map(strat_df, # Iterate through all strategies
                function(x) {
                  non_date_col <- names(x)[names(x) != "date"]
                  map(non_date_col, # Iterate through all non_date_columns
                      function(y) {
                        x[, c("date", y)] %>%  # Extract out date and asset size column
                          setNames(c("date", "size")) %>%
                          left_join(asset_returns[, c("date", y)] %>% setNames(c("date", "return")), by = "date") %>%  # Join with returns based on date
                          mutate(wt_return = size * return) %>%  # Calculated weighted return
                          select(wt_return) %>%   # Select just weighted return
                          setNames(y)    # Set name of weighted return to asset name
                      }
                  ) %>%
                    reduce(cbind) %>%
                    rowSums(na.rm = TRUE)    # Combine asset returns of each strategy into a single return stream
                })),
          names(strat_df))
  )

}

calc_strat_headline_size <- function(cleaned_size_strat_df) {
  cbind(date = cleaned_size_strat_df[[1]]$date,
        map(cleaned_size_strat_df,
            ~apply(select(., -date), 1,
                   function(y) {
                     max(sum(y[y > 0]), abs(sum(y[y < 0])))
                   })
        ) %>% as.data.frame %>% setNames(names(cleaned_size_strat_df))
  )
}

calc_strat_unwt_return <- function(wt_returns, strat_headline_size) {
  strat_headline_size <- strat_headline_size[,names(wt_returns)]
  returns <- remove_date(wt_returns) / remove_date(strat_headline_size)
  returns[is.na(returns)] <- 0

  cbind(date = wt_returns$date,
        returns)
}

get_strat_size <- function(strat_df, dt = NULL, approx = TRUE) {
  if (is.null(dt))
    dt <- max(strat_df$date)

  if (approx) {
    filtered_strat <- strat_df %>%
      mutate(diff = dt - date) %>%
      filter(diff >= 0) %>%
      arrange(diff) %>%
      head(1) %>%
      select(-diff)
  } else {
    filtered_strat <- strat_df %>%
      filter(date == dt)
  }
  filtered_strat %>%
    remove_date %>%  # Remove date column
    gather(strat, size) %>%   # Remove any sizes = 0
    filter(size != 0) %>%
    spread(strat, size)
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
#' calc_total_ret(df)
calc_total_ret <- function(df, return_date = FALSE, na.rm = FALSE) {
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
#' calc_returns(df)
calc_returns <- function(df) {
  has_date <- !is.null(df$date)
  if (has_date) df <- remove_date(df)
  # calc returns
  df <- df / mutate_all(df, funs(lag)) -1

  if (has_date) cbind(date = as.Date(row.names(df)), df)
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
calc_active_risk <- function(unwt_ret_w_date, curr_wt, start_date = today()-years(10), end_date = today(), annualize_factor = 250) {
  # Check all strategies in curr_wt have returns
  strats <- names(curr_wt)[names(curr_wt) != "date"]
  unfound_strats <- strats[! strats %in% names(unwt_ret_w_date)]
  if (length(unfound_strats) > 0)
    stop(paste("Strategy's return not found:",paste(unfound_strats, collapse = ",")))

  # If weights provided as data.frame, convert to vector for manipulation purposes
  if (class(curr_wt) == "data.frame") {
    wt <- remove_date(curr_wt)
    nam <- colnames(wt)
    curr_wt <- as.numeric(curr_wt)
    names(curr_wt) <- nam
  }

  unwt_ret_w_date <- unwt_ret_w_date %>% filter(date > start_date & date <= end_date)

  # Check if sufficient data is available
  if (nrow(unwt_ret_w_date) < 5) {
    stop("Less than 5 periods of data found, calculations will be invalid, please check the dates in return data vs the dates of analysis")
  }
  else if (nrow(unwt_ret_w_date) < 20) {
    warnings("Less than 20 periods of data found, calculations may not be reliable")
  }
  unwt_ret_fn <- unwt_ret_w_date %>% remove_date()

  # Filter only those strategies with weights
  unwt_ret_fn <- unwt_ret_fn[,names(curr_wt)]

  # calc active risk
  cov_matrix <- cov(unwt_ret_fn, use = "complete.obs") * annualize_factor # annualized

  numerator <- as.numeric(t(curr_wt) %*% cov_matrix)
  names(numerator) <- names(unwt_ret_fn)

  port_var <- as.numeric(t(curr_wt) %*% cov_matrix %*% curr_wt)
  port_sd <- sqrt(port_var)

  marginal_risk <- numerator / port_sd
  active_risk <- marginal_risk * curr_wt

  names(marginal_risk) <- names(curr_wt)
  list(parameters = list(start_date = start_date,
                         end_date = end_date,
                         weights = curr_wt),
         cov_matrix = cov_matrix, port_sd = port_sd, active_risk = active_risk, marginal_risk = marginal_risk)
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
