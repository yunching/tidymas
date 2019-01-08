utils::globalVariables(".")

#' @import dplyr
#' @import ggplot2
#' @importFrom purrr map map2 reduce
#' @importFrom stats setNames median
#' @importFrom lubridate today years days ymd
#' @importFrom tidyr gather spread fill replace_na
#' @importFrom Rblpapi bdh bdp
#' @importFrom utils read.csv head tail
#' @importFrom tibble as.tibble

NULL

#' Wrapper for bdh with pre-built options for getting daily data for all weekdays, non-trading weekdays will have previous value
#'
#' @param field A character vector with Bloomberg query fields.
#' @param start_date A Date variable with the query start date.
#' @param series A dataframe containing multiple securities with column names `ticker` and optional `name`
#' @param options_overrides A list containing any additional options besides those to get weekdays
#' @param end_date An optional Date variable with the query end date; if omitted the most recent available date is used.
#'
#' @return A list with as a many entries as there are entries in securities; each list contains a data.frame with one row per observations and as many columns as entries in fields. If the list is of length one, it is collapsed into a single data frame. Note that the order of securities returned is determined by the backend and may be different from the order of securities in the securities field.
#' @export
#'
#' @examples
#' \donttest{
#' bdh_weekday(c("SPX Index", "STI Index"), "PX_LAST", start.date = Sys.Date() - 31)
#' }
bdh_weekday <- function(series, field = "PX_LAST", start_date = as.Date("1994-01-01"), end_date = NULL, options_overrides = NULL) {
  # Set options
  defaults <- c("periodicitySelection" = "DAILY",
                "nonTradingDayFillOption" = "NON_TRADING_WEEKDAYS",
                "nonTradingDayFillMethod" = "PREVIOUS_VALUE")
  options <- if (is.null(options_overrides)) defaults else c(options_overrides, defaults[! names(defaults) %in% names(options_overrides)])

  ## ADDITIONAL OPTIONS FOR REFERENCE
  # option.fields <- c("periodicitySelection", "nonTradingDayFillOption",
  #                    "nonTradingDayFillMethod", "periodicityAdjustment",
  #                    "adjustmentFollowDPDF", "currency")
  #
  # option.values <- c("DAILY", "NON_TRADING_WEEKDAYS", "NIL_VALUE",
  #                    "CALENDAR", "TRUE", "USD")

  # If series is data.frame, match name to ticker
  if (any(class(series) == 'data.frame')) {
    if (is.null(series$ticker)) stop("securities is a dataframe, column 'ticker' is missing")

    bbg_series <- data.frame(ticker = series$ticker, stringsAsFactors = FALSE)
    bbg_series$name <- if (is.null(series$name)) series$ticker else series$name
  }
  # If series is just a character vector, search for name of vector, otherwise just use ticker as header
  else {
    bbg_series <- data.frame(ticker = series, stringsAsFactors = FALSE)
    bbg_series$name <- if (is.null(names(series))) series else names(series)
  }

  data <- bdh(bbg_series$ticker, field, start.date = start_date, end.date = end_date, options = options)

  if (length(field) == 1) {
    if (length(bbg_series$ticker) > 1) {
    df <- data %>% reduce(inner_join, by = "date")
    df <- df %>% setNames(c("date", data.frame(ticker = names(data), stringsAsFactors = FALSE) %>% left_join(bbg_series, by = "ticker") %>% .$name))
    }
    else {
      df <- data %>% setNames(c("date", bbg_series$name))
    }
  }
  else
    df <- data
  df
}

# Remove the date column from a dataframe and store it into rownames (used for preprocessing of dataframes for mass calculations e.g. returns)
remove_date <- function(df) {
  if ("date" %in% names(df)) {
    df <- as.data.frame(df)
    if (length(df$date) == length(unique(df$date)))
      rownames(df) <- as.Date(df$date)
    df <- df %>% select(-date)
  }
  df
}

# Generate a vector of weekdays from start_date to end_date
gen_weekdays <- function(start_date, end_date) {
  if (is.na(end_date)) end_date <- today()
  if (is.na(start_date)) start_date <- today()

  dat <- seq.Date(start_date, end_date, 1)
  dat[!weekdays(dat) %in% c("Saturday", "Sunday")]
}

#' Get tickers for the specific asset class from the package's csv files
#'
#' @param asset_cl A character variable of `govt`, `fut`, `equity`, `cds`, or `fx`
#' @param roll_differencing A boolean indicating if ticker for futures should be modified to account for roll_differencing, defaults as TRUE
#'
#' @return A dataframe containing the tickers and their corresponding identifiers
#' @export
#'
#' @importFrom stringr str_replace
#'
#' @examples
#' get_tickers("fx")
get_tickers <- function(asset_cl, roll_differencing = TRUE) {
  if (!asset_cl %in% c("govt", "fut", "equity", "cds", "fx"))
    stop("Invalid asset_class, only can handle govt, fut, equity, cds, fx")

  if (asset_cl == "fx") {
    output <- read.csv(system.file("extdata", "tickers_funding.csv", package = "tidymas"), stringsAsFactors = FALSE)
    if (roll_differencing) {
      # Roll differencing, see ‘DOCS #2072138 <GO> on Bloomberg
      output <- output %>% mutate(ticker = str_replace(.data$ticker, "(?i) comdty", " B:00_0_D Comdty"))
    }
  }
  else {
    output <- read.csv(system.file("extdata", "tickers_assets.csv", package = "tidymas"), stringsAsFactors = FALSE)
    if (asset_cl == "govt")
      asset_cl <- c("govt", "ilb")
    output <- output %>% filter(.$asset_class %in% asset_cl)
    output[output == ""] <- NA
  }

  output
}

#' Get tickers of available securities in the specific asset class, and check which securities in the instruments dataframe are valid for the asset class
#'
#' @param instruments_df A dataframe containing the instruments to search the tickers for. Dataframe must have the `asset_class` and `identifier` columns
#' @param type A character which can be `price` or `duration`, which indicates whether to return tickers for price or duration. Defaults to `price`
#'
#' @return A dataframe with the matched instruments from instruments_df and their corresponding tickers
#' @export
#'
#' @examples
#' inst <- data.frame(asset_class = c("equity", "ilb", "govt"),
#'                     identifier = c("spx", "germany_ilb_5y", "us_govt_10y"),
#'                     stringsAsFactors = FALSE)
#' get_and_check_tickers(inst)
get_and_check_tickers <- function(instruments_df, type = c("price", "duration")) {
  if (length(type) == 2) {
    type <- type[1]
  }

  sec_list <- map(list("govt", "equity", "cds", "fut"), get_tickers) %>%
    reduce(rbind)

  output <- instruments_df %>%
    group_by(.data$asset_class, .data$identifier) %>%
    summarise() %>%
    left_join(sec_list, by = c("asset_class", "identifier"))

  non_fx_unfound <- filter(output, .data$asset_class != "fx" & is.na(.data$ticker_price))
  if (nrow(non_fx_unfound) > 0) {
    warning(paste("Tickers not found for assets:", paste(mutate(non_fx_unfound, error_msg = paste(.data$asset_class, .data$identifier, sep = "/"))$error_msg, collapse = ",")))
  }

  output %>% rename(name = "identifier", ticker = ifelse(type == "duration", "ticker_duration", "ticker_price"))
}

# Checks if strategy input file has errors
check_strategy_inputs <- function(strategies) {
  # Check strategies and type
  type_error <- strategies %>%
    group_by(.data$strategy, .data$type) %>%
    summarise() %>% ungroup %>%
    group_by(.data$strategy) %>%
    summarise(n = n()) %>%
    filter(.data$n > 1)

  if (nrow(type_error) > 0) {
    stop(paste("Multiple `type` found for strategies:",
               paste(type_error$strategy, collapse = ","),
               ". See rows: ",
               paste(strategies %>% mutate(row_num = rownames(.)) %>%  filter(.data$strategy %in% type_error$strategy) %>% .$row_num , collapse = ",")))
  }

  # Check size_type
  valid_size_type <- c("months", "percent")
  size_types <- strategies$size_type %>% unique
  if (mean(size_types %in% valid_size_type) < 1)
    stop(paste("Invalid size_type found in input (only `months` and `percent` allowed):", paste(size_types[! size_types %in% valid_size_type], collapse = ",")))

  size_warning <-  strategies %>%
    group_by(.data$asset_class, .data$size_type) %>%
    summarise() %>%
    mutate(warning = ifelse((.data$asset_class %in% c("cds", "fut", "govt", "ilb") & .data$size_type != "months") |
                            (.data$asset_class %in% c("equity", "fx") &.data$ size_type != "percent"), 1, 0)) %>%
    filter(.data$warning != 0)

  if (nrow(size_warning) > 0) {
    show_warning_rows <- strategies %>% left_join(size_warning, by = c("asset_class", "size_type")) %>%
      mutate(warning_msg = paste0("Row ", rownames(.), ":", .data$asset_class, "/", .data$size_type)) %>%
      filter(!is.na(.data$warning))
    warning(paste("Potentially wrong size_type found", paste(show_warning_rows$warning_msg, collapse = ",")))
  }

  # Check asset classes
  valid_asset_class <- c("govt", "ilb", "fx", "equity", "cds", "fut")
  asset_classes <- strategies$asset_class %>% unique
  if (mean(asset_classes %in% valid_asset_class) < 1)
    stop(paste("Invalid asset class found in input:", paste(asset_classes[! asset_classes %in% valid_asset_class], collapse = ",")))

  # Check if differing size_types for same instrument in the same strategy
  unique_sizes_for_instrument <- strategies %>%
    group_by(.data$strategy, .data$identifier, .data$size_type) %>%
    summarise() %>%
    ungroup() %>%
    group_by(.data$strategy, .data$identifier) %>%
    summarise(count = n())

  if (any(unique_sizes_for_instrument$count > 1)) {
    invalid_strategies <- subset(unique_sizes_for_instrument, count > 1) %>%
      mutate(error_msg = paste(.data$strategy, .data$instrument, sep = "/"))
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
    filter(.data$asset_class == "fx") %>%
    mutate(left_curr = substr(.data$identifier, 1, 3),
           right_curr = substr(.data$identifier, 4, 6),
           error = (!((.data$left_curr %in% fx_tickers$currency) & (.data$right_curr %in% fx_tickers$currency))) | (nchar(.data$identifier) != 6)) %>%
    filter(.data$error)
  if (nrow(fx_errors) > 0)
    stop(paste("Invalid fx identifiers:", paste(fx_errors$identifier, collapse = ", "), ". FX identifiers need to be in small caps. For valid identifiers, see `?get_tickers`"))

  # Check tickers for CDS
  cds_errors <- strategies %>%
    filter(.data$asset_class == "cds") %>%
    left_join(cds_tickers, by = c("asset_class", "identifier")) %>%
    filter(is.na(.data$ticker_price))

  if (nrow(cds_errors) > 0) {
    stop(paste("Invalid cds identifiers:", paste(cds_errors$identifier, collapse = ","), ". For valid identifiers, see `?get_tickers`"))
  }

  # Check tickers exist for other instruments
  strat_inst <- strategies %>%
    filter(.data$asset_class %in% c("govt", "ilb", "fut", "equity")) %>%
    group_by(.data$identifier, .data$asset_class) %>%
    summarise %>%
    left_join(non_fxcds_tickers, by = c("asset_class", "identifier"))

  if (any(is.na(strat_inst$ticker_price))) {
    not_found <- strat_inst %>% filter(is.na(.data$ticker_price)) %>%
      mutate(error_msg = paste(.data$asset_class, .data$identifier, sep = "/"))
    stop(paste("Identifier not found: ", paste(not_found$error_msg, collapse = ","), ". For valid identifiers, see `?get_tickers`"))
  }

  # Check duplicate entries
  dup_entries <- strategies %>%
    mutate(row_n = rownames(.)) %>%
    group_by(.data$owner, .data$strategy, .data$type, .data$open_date, .data$close_date,
                               .data$identifier, .data$asset_class, .data$size_type) %>%
    summarise(dup = paste(.data$row_n, collapse = ","), n = n()) %>%
    filter(n > 1)

  if (nrow(dup_entries) > 0) {
    warning(paste("Potential duplicates found, rows:", paste(dup_entries$dup, collapse = " / ")))
  }
}

#' Build strategies from an input csv file or dataframe of strategies, can use `demo_strategies` to generate a template file
#'
#' @param input A dataframe containing strategy inputs or A character variable containing the path to the input csv file
#' @param start_date A Date variable with start date of generation, this can be earlier or later than the dates in the template file
#' @param end_date A Date variable containing the end date for the position sizes, defaults to today
#'
#' @return A list containing the actual sizes (mix of months-weighted and percentage based on input file) and simulated sizes ie actual sizes but filled upwards, then downwards to use for simulation of returns
#' @export
#'
#' @examples
#' data(demo_strategies)
#' build_strategies(demo_strategies, as.Date("2016-01-01"), as.Date("2018-12-07"))
#' \donttest{
#' build_strategies("input_file.csv")
#' }
build_strategies <- function(input, start_date = as.Date("2000-01-01"), end_date = today()) {
  # Read in strategies
  if (class(input) == "character") {
    if (is.character(input) && file.exists(input)) {
      strategies <- read.csv(input, stringsAsFactors = FALSE)
    } else {
      stop(sprintf("File %s cannot be found", input))
    }
  } else if ("data.frame" %in% class(input)) {
    strategies <- input
  } else{
    stop("Invalid input, input can only be dataframe or character containing file path")
  }

  strategies <- strategies %>%
    mutate(open_date = ymd(.data$open_date), close_date = ymd(.data$close_date),
           strategy = paste(.data$strategy, .data$owner, sep = ":::"))

  check_strategy_inputs(strategies)

  ## Iterate through each unique strategy
  list_of_strat <- map(unique(strategies$strategy), function(i) {
    # Filter out current strategy
    curr_strat <- strategies %>% filter(.data$strategy == i)
    inst_list <- curr_strat$identifier %>% unique
    actual_size <- data.frame(date = gen_weekdays(start_date, end_date))

    # Iterate through all identifiers required for this strategy
    for (curr_inst in inst_list) {
      curr_inst_df <- curr_strat %>% filter(.data$identifier == curr_inst)

      actual_size[[curr_inst]] <- 0

      # Set weight as actual first
      for (j in 1:nrow(curr_inst_df)) {
        # Strat start and end
        strat_start <- if (is.na(curr_inst_df$open_date[j])) end_date - 1 else curr_inst_df$open_date[j]
        strat_end <- if (is.na(curr_inst_df$close_date[j])) end_date else curr_inst_df$close_date[j]

        actual_size[[curr_inst]] <- actual_size[[curr_inst]] +
          if_else(actual_size$date <= strat_end & actual_size$date > strat_start,
                  curr_inst_df$size[j], 0)
      }
    }

    # Calculate simulated size
    sim_size <- actual_size %>%
      mutate(has_position = sign(rowSums(abs(.[-1])))) %>%  # Check if there's any position at the time
      gather("asset", "size", -.data$date, -.data$has_position) %>%  # convert into gathered dataframe
      mutate(size = ifelse(.data$has_position, .data$size, NA)) %>%   # if does not have any position, replace with NA
      spread(.data$asset, .data$size) %>%  # Convert back into original layout
      select(-.data$has_position) %>%
      fill(-.data$date, .direction = "up")  %>%
      fill(-.data$date, .direction = "down") # Fill up, then down to times without position, to calculate returns for simulation and correlations

    # Mutate into tidy form
    actual_size <- actual_size %>%
      mutate(strategy = i) %>%
      gather("instrument", "size", -.data$date, -.data$strategy)

    sim_size <- sim_size %>%
      mutate(strategy = i) %>%
      gather("instrument", "size", -.data$date, -.data$strategy)


    list(actual = actual_size,
         sim = sim_size)
  })

  all_actual_size <- map(list_of_strat, ~.$actual) %>%
    reduce(rbind) %>%
    as.tibble

  all_sim_size <- map(list_of_strat, ~.$sim) %>%
    reduce(rbind) %>%
    as.tibble

  # Summarise strategies (for use in size conversions later)
  strat_summ <- strategies %>%
    group_by(.data$strategy, .data$identifier, .data$asset_class, .data$size_type, .data$owner, .data$type) %>%
    summarise() %>%
    ungroup()

  list(summary = strat_summ, actual = all_actual_size, sim = all_sim_size)
}

# Get duration of bonds from Bloomberg, end user should avoid using this but use `get_dur_bbg` instead
get_dur_bonds_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today(), fill = TRUE) {
  reduced_sec_df <- get_and_check_tickers(instruments_df, type = "duration")

  message("Downloading bond duration from bbg...")
  dur_df <- bdh_weekday(reduced_sec_df, "MODIFIED_DURATION", start_date = start_date, end_date = end_date)

  # Some instruments have very short duration history e.g. ILBs, hence we just assume all previous to be the first duration.
  if (fill)
    dur_df <- fill(dur_df, everything(), .direction = "up")

  dur_df
}

# Get duration of futures from Bloomberg, end user should avoid using this but use `get_dur_bbg` instead
get_dur_fut_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today(), fill = TRUE) {
  reduced_sec_df <- get_and_check_tickers(instruments_df, type = "duration")

  # Download data from Bloomberg
  message("Downloading futures duration from bbg...")
  dur_df <- bdh_weekday(reduced_sec_df, "FUT_EQV_DUR_NOTL", start_date = start_date, end_date = end_date)

  # Download duration where only most recent is available e.g. Euribors
  all_nas <- dur_df %>% gather("identifier", "duration", -.data$date) %>%
    group_by(.data$identifier) %>%
    summarise(na = mean(is.na(.data$duration))) %>%
    filter(.data$na == 1) %>%
    left_join(reduced_sec_df, by = c("identifier"="name"))

  recent_dur <- bdp(all_nas$ticker, "FUT_EQV_DUR_NOTL")
  recent_dur$identifier <- all_nas$identifier

  for (i in 1:nrow(recent_dur)) {
    dur_df[[recent_dur$identifier[i]]] <- recent_dur[i, 1]
  }

  # Some instruments have very short duration history e.g. ILBs, hence we just assume all previous to be the first duration.
  if (fill)
    dur_df <- fill(dur_df, everything(), .direction = "up")

  dur_df
}

# Get duration of cds from Bloomberg, end user should avoid using this but use `get_dur_bbg` instead
get_dur_cds_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today(), fill = TRUE) {
  reduced_sec_df <- get_and_check_tickers(instruments_df, type = "duration")

  # Download data from Bloomberg
  message("Downloading cds duration from bbg...")
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

#' Get time-series duration of asset from Bloomberg. It is a generic function which means new duration functions can be easily added for new assets
#'
#' @param instruments_df A dataframe containing the columns `identifier` and `asset_class`
#' @param start_date A Date variable
#' @param end_date A Date variable
#' @param fill A boolean indicating whether NAs in the data should be filled, data is only filled upwards
#'
#' @return A dataframe containing the the time-series duration of the assets, with the identifier as the name
#' @export
#'
#' @examples
#' \donttest{
#' inst <- data.frame(asset_class = c("ilb", "govt", "fut", "cds"),
#'    identifier = c("germany_ilb_5y", "us_govt_10y", "fut_rx1", "cdx_us_ig"),
#'    stringsAsFactors = FALSE)
#' get_dur_bbg(inst)
#' }
get_dur_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today(), fill = TRUE) {
  sec_df <- instruments_df %>%
    group_by(.data$identifier, .data$asset_class) %>%
    summarise() %>%
    ungroup

  bonds <- sec_df %>% filter(.data$asset_class %in% c("govt", "ilb"))
  fut <- sec_df %>% filter(.data$asset_class == "fut")
  cds <- sec_df %>% filter(.data$asset_class == "cds")

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

  dur_all %>%
    gather("instrument", "duration", -date) %>%
    as.tibble
}

#' Converts size in months to size in percent of portfolio terms
#'
#' @param strat_df A gathered dataframe containing `date`, `strategy`, `instrument`, and `size` in percent or month-weighted sizes.
#' @param strat_id_sizetype A dataframe containing the columns of `strategy`, `identifier` and `size_type` that correspondings to those in strat_df. `size_type` must be `months` or `percent`
#' @param duration_df A dataframe containing timeseries duration of assets, can be generated from `get_dur_bbg`
#' @param convert_to_decimal A boolean indicating if all final percent numbers be converted to decimal form ie. divide by 100
#'
#' @return A dataframes with same structure as `strat_df`, but sizes converted to percent form
#' @export
#'
#' @examples
#' data(demo_strategies)
#' data(demo_duration)
#' portfolios <- build_strategies(demo_strategies, as.Date("2016-01-01"), as.Date("2018-12-07"))
#' actual_pf_size <- convert_dur_size(portfolios$actual, portfolios$summary, demo_duration)
convert_dur_size <- function(strat_df, strat_id_sizetype, duration_df, convert_to_decimal = TRUE) {

  # Check if there are any missing duration
  missing_duration <- strat_id_sizetype %>%
    filter(.data$size_type == "months" & (! .data$identifier %in% unique(duration_df$instrument))) %>%
    .$identifier %>%
    unique
  if (length(missing_duration) > 0) stop(paste("duration missing:", paste(missing_duration, collapse = ",")))

  # Check for missing dates
  if (any(!strat_df$date %in% duration_df$date)) {
    missing_dates <- subset(strat_df, ! date %in% duration_df$date) %>%
      .$date
    stop("Duration missing data for dates: ", paste(missing_dates, collapse = ","))
  }

  output <- strat_df %>%
    left_join(duration_df, by = c("date", "instrument")) %>%
    left_join(
      select(strat_id_sizetype, .data$strategy, instrument = .data$identifier, .data$size_type),
      by = c("strategy", "instrument")) %>%
    mutate(size = ifelse(.data$size_type == "percent", .data$size, .data$size / (.data$duration * 12) * 100)) %>%
    select(-.data$duration, -.data$size_type)

  if (convert_to_decimal)
    output$size <- output$size / 100

  output
}

# Get daily return of bonds from Bloomberg, end user should avoid using this but use `get_ret_bbg` instead
get_ret_bonds_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  reduced_sec_df <- get_and_check_tickers(instruments_df)

  message("Downloading bond prices from bbg...")
  price_df <- bdh_weekday(reduced_sec_df, "PX_LAST", start_date = start_date, end_date = end_date)

  calc_returns(price_df)
}

# Get daily return of FX from Bloomberg, including depo returns where available. End user should avoid using this but use `get_ret_bbg` instead
get_ret_fx_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  reduced_fx <- instruments_df %>% filter(.data$asset_class == "fx") %>%
    mutate(left_curncy = substr(.data$identifier, 1, 3),
           right_curncy = substr(.data$identifier, 4, 6),
           ticker = paste(toupper(.data$identifier), "Curncy"))

  # Calculate price return
  req_fx <- reduced_fx %>%
    group_by(.data$identifier, .data$ticker) %>%
    summarise() %>%
    mutate(name = .data$identifier)

  message("Downloading fx prices...")
  fx_index <- bdh_weekday(req_fx, start_date = start_date, end_date = end_date)

  fx_price_ret <- calc_returns(fx_index)

  # Download funding rates from Bloomberg
  req_curncy <- unique(c(reduced_fx$left_curncy, reduced_fx$right_curncy))

  req_fx_depo <- get_tickers("fx") %>%
    filter(.data$currency %in% req_curncy) %>%
    mutate(name = .data$currency)

  message("Downloading depo rates...")
  fx_funding_rates <- bdh_weekday(req_fx_depo, start_date = start_date, end_date = end_date)

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

# Get daily return of equity indices from Bloomberg, end user should avoid using this but use `get_ret_bbg` instead
get_ret_equity_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  reduced_sec_df <- get_and_check_tickers(instruments_df)

  message("Downloading equity prices from bbg...")
  price_df <- bdh_weekday(reduced_sec_df, "PX_LAST", start_date = start_date, end_date = end_date)

  calc_returns(price_df)
}

# Get daily return of cds from Bloomberg, end user should avoid using this but use `get_ret_bbg` instead
get_ret_cds_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  reduced_sec_df <- get_and_check_tickers(instruments_df, type = "price") %>%
    mutate()

  message("Downloading cds prices from bbg...")
  price_df <- bdh_weekday(reduced_sec_df, "PX_LAST", start_date = start_date, end_date = end_date)

  calc_returns(price_df)
}

# Get daily return of futures from Bloomberg, end user should avoid using this but use `get_ret_bbg` instead
get_ret_fut_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  reduced_sec_df <- get_and_check_tickers(instruments_df)

  message("Downloading futures prices from bbg...")
  price_df <- bdh_weekday(reduced_sec_df, "CONTRACT_VALUE", start_date = start_date, end_date = end_date)
  calc_returns(price_df)
}

#' Get time-series daily returns of asset from Bloomberg. It is a generic function which means new return functions can be easily added for new assets
#'
#' @param instruments_df A dataframe containing the columns `asset_class` and `identifier`
#' @param start_date A Date variable
#' @param end_date A Date variable
#'
#' @return A gathered dataframe containing timeseries daily returns with columns `date`, `instrument` and `return`
#' @export
#'
#' @examples
#' \donttest{
#' inst <- data.frame(asset_class = c("ilb", "govt", "fut", "cds"),
#'     identifier = c("germany_ilb_5y", "us_govt_10y", "fut_rx1", "cdx_us_ig"),
#'     stringsAsFactors = FALSE)
#' get_ret_bbg(inst)
#' }
get_ret_bbg <- function(instruments_df, start_date = as.Date("1994-01-01"), end_date = today()) {
  sec_df <- instruments_df %>%
    group_by(.data$identifier, .data$asset_class) %>%
    summarise() %>%
    ungroup()

  bonds <- filter(sec_df, .data$asset_class %in% c("govt", "ilb"))
  fut <- filter(sec_df, .data$asset_class == "fut")
  cds <- filter(sec_df, .data$asset_class == "cds")
  fx <- filter(sec_df, .data$asset_class == "fx")
  equity <- filter(sec_df, .data$asset_class == "equity")

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

  ret_all %>%
    gather("instrument", "return", -.data$date) %>%
    as.tibble
}

#' Calculate weighted return of strategies
#'
#' @param strat_df A dataframes for each strategy with their corresponding timeseries sizes in percent or month-weighted sizes. Columns should be `date`, `strategy`, `instrument`, `size`
#' @param asset_returns A dataframe with time-series returns of all instruments with column name as the `identifier`
#'
#' @return A dataframe containing the time-series daily return of all instruments
#' @export
#'
#' @examples
#' portfolios <- build_strategies(demo_strategies, as.Date("2016-01-01"), as.Date("2018-12-07"))
#' dur <- demo_duration
#' actual_pf_size <- convert_dur_size(portfolios$actual, portfolios$summary, dur)
#' ret <- demo_return
#' calc_strat_wt_return(actual_pf_size, ret)
#' \donttest{
#' # With Bloomberg
#' portfolios <- build_strategies(demo_strategies)
#' dur <- get_dur_bbg(portfolios$summary)
#' actual_pf_size <- convert_dur_size(portfolios$actual, portfolios$summary, dur)
#' ret <- get_ret_bbg(portfolios$summary)
#' calc_strat_wt_return(actual_pf_size, ret)
#' }
calc_strat_wt_return <- function(strat_df, asset_returns) {
  strat_df %>% left_join(asset_returns, by = c("date", "instrument")) %>%
    mutate(wt_return = .data$size * .data$return) %>%
    group_by(.data$date, .data$strategy) %>%
    summarise(wt_return = sum(.data$wt_return, na.rm = TRUE)) %>%
    select(.data$date, .data$strategy, .data$wt_return) %>%
    arrange(.data$strategy, .data$date) %>%
    ungroup
}

#' Calculates headline sizes of strategies given the size of the component trades by first summing the positive sizes and negative sizes separately. Then taking the higher number as the size
#'
#' @param strat_df A dataframe of strategies which are dataframes containing timeseries sizes of their component instruments. Columns should be `date`, `strategy`, `instrument`, `size`
#'
#' @return A dataframe containing the size of the strategies daily
#' @export
#'
#' @examples
#' portfolios <- build_strategies(demo_strategies, as.Date("2016-01-01"), as.Date("2018-12-07"))
#' actual_pf_size <- convert_dur_size(portfolios$actual, portfolios$summary, demo_duration)
#' calc_strat_headline_size(actual_pf_size)
calc_strat_headline_size <- function(strat_df) {
  strat_df %>% spread(.data$instrument, .data$size) %>%
    mutate(pos_sum = rowSums((.[c(-1,-2)] > 0) * .[c(-1,-2)], na.rm = T),
           neg_sum = rowSums((.[c(-1,-2)] < 0) * .[c(-1,-2)], na.rm = T),
           size = pmax(.data$pos_sum, abs(.data$neg_sum))) %>%
    select(.data$date, .data$strategy, .data$size) %>%
    arrange(.data$strategy, .data$date)
}

#' Calculate unweighted returns given weighted returns and size of strategies
#'
#' @param wt_return A dataframe containing the timeseries weighted return of the strategies, recommended to be simulated returns, so that unweighted return for all of history can be obtained
#' @param strat_headline_size A dataframe containing the timeseries headline sizes of the trades. Should be all in percent. Recommended to be simulated sizes, needs to correspond to `wt_return`
#'
#' @return A dataframe containing the timeseries unweighted return of the strategies
#' @export
#'
#' @examples
#' portfolios <- build_strategies(demo_strategies, as.Date("2016-01-01"), as.Date("2018-12-07"))
#' sim_pf_size <- convert_dur_size(portfolios$sim, portfolios$summary, demo_duration)
#' wt_return <- calc_strat_wt_return(sim_pf_size, demo_return)
#' headline_size <- calc_strat_headline_size(sim_pf_size)
#' calc_strat_unwt_return(wt_return, headline_size)
#' \donttest{
#' # With Bloomberg
#' portfolios <- build_strategies(demo_strategies)
#' dur <- get_dur_bbg(portfolios$summary)
#' sim_pf_size <- convert_dur_size(portfolios$sim, portfolios$summary, dur)
#' ret <- get_ret_bbg(portfolios$summary)
#' wt_return <- calc_strat_wt_return(sim_pf_size, ret)
#' headline_size <- calc_strat_headline_size(sim_pf_size)
#' calc_strat_unwt_return(wt_return, headline_size)
#' }
calc_strat_unwt_return <- function(wt_return, strat_headline_size) {
  wt_return %>% left_join(strat_headline_size, by = c("date", "strategy")) %>%
    mutate(return = .data$wt_return / .data$size) %>%
    select(.data$date, .data$strategy, .data$return) %>%
    replace_na(list(return = 0)) %>%
    ungroup
}

#' Group returns of strategies
#'
#' @param returns_df A dataframe containing timeseries of returns of strategies with columns `date`, `strategy`, `return`
#' @param instr_df A dataframe containing `strategy` corresponding to any groupings
#' @param group A character variable containing the name of the column in `instr_df` to group by
#'
#' @return A dataframe containing timeseries of returns of the groups
#' @export
custom_grouping <- function(returns_df, instr_df, group) {
  grouped_instr_df <- instr_df %>% select_("strategy", group)
  tmp <- returns_df %>%
    left_join(grouped_instr_df, by = "strategy") %>%
    group_by_("date", group)

  if ("return" %in% names(tmp)) {
    tmp %>%
      summarise(return = sum(.data$return, na.rm = TRUE)) %>%
    ungroup()
  } else {
    tmp %>%
      summarise(wt_return = sum(.data$wt_return, na.rm = TRUE)) %>%
      ungroup()
    }
}

#' Gets a sizes of each strategy in a portfolio at a specific date. Use to retrieve sizes in month weight terms
#'
#' @param portfolio A dataframe of strategies which are timeseries sizes of their component instruments, contains columns `date`, `strategy`
#' @param as_of_date A Date variable
#' @param approx A boolean indicating if `as_of_date` is unavailable, the next available earlier date should be used
#'
#' @return A dataframe containing the sizes of the strategies
#' @export
pf_summary <- function(portfolio, as_of_date = NULL, approx = TRUE) {
  if (is.null(as_of_date)) {
    as_of_date <- portfolio$date %>% tail(1)
  }

  sz <- calc_strat_headline_size(portfolio)
  get_strat_size(sz, as_of_date, approx)
}

#' Get strategies sizes at a specific date from headline sizes of strategies
#'
#' @param strat_df A dataframe containing timeseries of headline sizes of strategies
#' @param as_of_date A Date variable
#' @param approx A boolean indicating if `as_of_date` is not available, the next available earlier date will be used
#'
#' @return A dataframe containing the columns `strat` and `size`
#' @export
get_strat_size <- function(strat_df, as_of_date = NULL, approx = TRUE) {
  if (is.null(as_of_date)) {
    message("as_of_date is NULL, retrieving latest available date")
    as_of_date <- max(strat_df$date)
  }

  if (approx) {
    filtered_strat <- strat_df %>%
      mutate(diff = as_of_date - .data$date) %>%
      filter(.data$diff >= 0) %>%
      filter(.data$diff == min(.data$diff)) %>%
      select(-.data$diff)
  } else {
    filtered_strat <- strat_df %>%
      filter(.data$date == as_of_date)
  }

  filtered_strat %>%
    filter(.data$size != 0)
}

#' Calculate the returns in history given a set of static strategy weights
#'
#' @param unwt_ret A dataframe containing the timeseries unweighted return of strategies, with columns `date`, `strategy`, `return`
#' @param curr_wt A dataframe weights of strategies with columns `strategy`, `size`
#' @param start_date A Date variable indicating the start period of simulation
#' @param end_date A Date variable indicating the end period of simulation
#'
#' @return A dataframe containing the timeseries weighted return of the period
#' @export
#'
#' @examples
#' portfolios <- build_strategies(demo_strategies, as.Date("2016-01-01"), as.Date("2018-12-07"))
#' sim_pf_size <- convert_dur_size(portfolios$sim, portfolios$summary, demo_duration)
#' wt_return <- calc_strat_wt_return(sim_pf_size, demo_return)
#' headline_size <- calc_strat_headline_size(sim_pf_size)
#' unwt_return <- calc_strat_unwt_return(wt_return, headline_size)
#' curr_wt <- get_strat_size(headline_size, as.Date("2018-01-05"))
#' simulate_history(unwt_return, curr_wt, as.Date("2016-01-01"), as.Date("2018-01-01"))
simulate_history <- function(unwt_ret, curr_wt, start_date, end_date) {

  # Check all strategies in curr_wt have returns
  strats <- unique(curr_wt$strategy)
  unfound_strats <- strats[! strats %in% unique(unwt_ret$strategy)]
  if (length(unfound_strats) > 0)
    stop(paste("Strategy's return not found:",paste(unfound_strats, collapse = ",")))

  unwt_ret %>% left_join(remove_date(curr_wt), by = "strategy") %>%
    mutate(wt_return = .data$return * .data$size) %>%
  select(.data$date, .data$strategy, .data$wt_return)
}

#' Calculate individual daily returns of a dataframe containing multiple assets
#'
#' @param df dataframe containing asset prices, `date` column is optional
#' @param return_type character of `percent` or `dollar` indicating if the return values should be in percentage or dollar
#'
#' @return dataframe containing returns
#' @export
#'
#' @examples
#' df <- data.frame(spx = c(2850, 2820, 2890), ukx = c(7004, 7010, 7080))
#' calc_returns(df)
calc_returns <- function(df, return_type = "percent") {
  if (!return_type %in% c("percent", "dollar"))
    stop("Invalid return type, only 'percent' and 'dollar' allowed")
  has_date <- !is.null(df$date)
  if (has_date) df <- remove_date(df)

  # calc returns
  if (return_type == "dollar") {
    df <- df - mutate_all(df, funs(lag))
  } else {
    df <- df / mutate_all(df, funs(lag)) -1
  }

  if (has_date) cbind(date = as.Date(row.names(df)), df)
  else df
}

#' Calculate active risk given return of strategies and weight of the strategies
#'
#' @param unwt_ret dataframe of returns of strategies with columns `date`, `strategy`, `return`
#' @param curr_wt dataframe of weights with columns `strategy`, `size`
#' @param start_date start_date for calc of active risk
#' @param end_date end_date
#' @param annualize_factor factor to multiply by to convert to annual. 250 for daily data (default) and 12 for monthly data
#'
#' @return list containing `cov_matrix`, `port_sd` (portfolio std dev), `active_risk`, `marginal_risk`
#' @export
#'
#' @importFrom stats cov
#'
#' @examples
#' portfolios <- build_strategies(demo_strategies,
#'   start_date = as.Date("2016-01-01"),
#'   end_date = as.Date("2018-12-07"))
#' sim_pf_size <- convert_dur_size(portfolios$sim, portfolios$summary, demo_duration)
#' wt_return <- calc_strat_wt_return(sim_pf_size, demo_return)
#' headline_size <- calc_strat_headline_size(sim_pf_size)
#' unwt_return <- calc_strat_unwt_return(wt_return, headline_size)
#' curr_wt <- get_strat_size(headline_size, as.Date("2018-01-05"))
#' calc_active_risk(unwt_return, curr_wt, as.Date("2017-06-01"), as.Date("2018-06-01"))
calc_active_risk <- function(unwt_ret, curr_wt, start_date = today()-years(10), end_date = today(), annualize_factor = 250) {
  unwt_ret_w_date <- unwt_ret %>% spread(.data$strategy, .data$return)
  # Check all strategies in curr_wt have returns
  strats <- unique(curr_wt$strategy)
  unfound_strats <- strats[! strats %in% unique(unwt_ret$strategy)]
  if (length(unfound_strats) > 0)
    stop(paste("Strategy's return not found:",paste(unfound_strats, collapse = ",")))

  duplicates <- curr_wt %>% group_by(.data$date, .data$strategy) %>% summarise(n = n()) %>% filter(.data$n > 1)

  if (nrow(duplicates) > 0)
    stop(paste("Multiple entry for same strategy found:", paste(duplicates$strategy, collapse = ",")))

  wt <- curr_wt %>% select(.data$strategy, .data$size) %>%
    spread(.data$strategy, .data$size) %>%
    unlist

  unwt_ret_w_date <- unwt_ret_w_date %>% filter(.data$date > start_date & .data$date <= end_date)

  # Check if sufficient data is available
  if (nrow(unwt_ret_w_date) < 5) {
    stop("Less than 5 periods of data found, calculations will be invalid, please check the dates in return data vs the dates of analysis")
  } else if (nrow(unwt_ret_w_date) < 20) {
    warning("Less than 20 periods of data found, calculations may not be reliable")
  }
  unwt_ret_fn <- unwt_ret_w_date %>% remove_date()

  # Filter only those strategies with weights
  unwt_ret_fn <- unwt_ret_fn[,names(wt)]

  # calc active risk
  cov_matrix <- cov(unwt_ret_fn, use = "complete.obs") * annualize_factor # annualized

  numerator <- as.numeric(t(wt) %*% cov_matrix)
  names(numerator) <- names(unwt_ret_fn)

  port_var <- as.numeric(t(wt) %*% cov_matrix %*% wt)
  port_sd <- sqrt(port_var)

  mr <- numerator / port_sd
  ar <- mr * wt

  data.frame(strategy = names(ar), active_risk = ar)
}

# Function for
#' Calculate correlation between returns
#'
#' @param unwt_ret_df dataframe containing timeseries returns for each strategy
#' @param start_date A Date variable which is the start_date of the period for correlation calculation
#' @param end_date A Date variable
#' @param period_name optional period_name to attach to the returned correlations
#'
#' @return long form dataframe (see tidyr::gather) containing correlation between returns, and `period_name`` if provided
#' @export
#'
#' @importFrom stats cor
#' @importFrom psych mat.sort
#'
#' @examples
#' unwt_ret <- data.frame(date = as.Date(c("2018-01-02", "2018-01-03", "2018-01-04", "2018-01-05")),
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

  # Calculate correlation and sort by similar correlations
  cor_table <- cor(unwt_ret_df %>% remove_date, use = "pairwise.complete.obs") %>%
    psych::mat.sort()

  sorted_categories <- colnames(cor_table)

  df <- cor_table %>%
    clear_diag %>%
    as.data.frame

  df$strat1 <- rownames(df)

  df <- df %>%
    gather("strat2", "corr", -.data$strat1)

  if (!is.null(period_name)) {
    df <- df %>% mutate(period = period_name)
  }

  df %>%
    mutate(strat1 = factor(.data$strat1, levels = sorted_categories),
           strat2 = factor(.data$strat2, levels = sorted_categories))
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
#' unwt_ret <- data.frame(date = as.Date(c("2018-01-02", "2018-01-03", "2018-01-04", "2018-01-05")),
#'                               long_spx = c(0.015, 0.021, -0.03, 0.01),
#'                               long_ukx = c(-0.005, 0.03, -0.01, -0.04),
#'                               long_hsi = c(0.023, 0.001, -0.005, 0.008))
#' cor_df <- calc_cor(unwt_ret)
#' plot_cor(cor_df)
plot_cor <- function(cor_df, title = NULL) {
  print(
    cor_df %>%
      ggplot(aes(x = .data$strat1, y = .data$strat2)) +
      geom_tile(aes(fill = .data$corr)) +
      geom_text(aes(label=formatC(.data$corr, digits = 2, format = "f"))) +
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
#' library(ggplot2)
#' input_text <- "period asset_class active_risk\n
#'       Last3M Equity 1.256309e-05\n
#'       Last3M Fixed_Income 1.389163e-03\n
#'       Last3M FX 1.531547e-04\n
#'       Last3M Others 2.138171e-05\n
#'       GFCStress Equity 2.083595e-04\n
#'       GFCStress Fixed_Income 4.309154e-03\n
#'       GFCStress FX 9.310046e-04\n
#'       GFCStress Others 2.353732e-04\n
#'       TaperTantrum Equity 2.892491e-05\n
#'       TaperTantrum Fixed_Income 2.097302e-03\n
#'       TaperTantrum FX 1.601432e-04\n
#'       TaperTantrum Others 6.576230e-05"
#' input_table <- read.table(text=input_text, header = 1)
#'
#'
#' dat <- sort_gg(input_table, "asset_class", "active_risk")
#' ggplot(dat, aes(x = asset_class, y = active_risk * 10000)) +
#'   geom_col() +
#'   facet_wrap(~period, ncol = 3)
sort_gg <- function(df_gathered, group_by_column, sort_by, filter_scenario = "Last3M") {
  ordered_column <- df_gathered %>%
    filter(.data$period == filter_scenario) %>%
    group_by_(group_by_column) %>%
    summarise(stat = sum(!!sym(sort_by))) %>%
    arrange(desc(.data$stat)) %>%
    .[[group_by_column]] %>%
    as.character %>%
    rev

  df_gathered[[group_by_column]] <- factor(df_gathered[[group_by_column]], levels = ordered_column)
  df_gathered
}


#' demo_strategies
#' @name demo_strategies
#' @docType data
#' @details Demo strategies for use with active risk calculations
NULL

#' demo_duration
#' @name demo_duration
#' @docType data
#' @details Demo duration for use with conversion of size from months weighted to percent
NULL

#' demo_return
#' @name demo_return
#' @docType data
#' @details Demo return for instruments to use for return calculations
NULL
