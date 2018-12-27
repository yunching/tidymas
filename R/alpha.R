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
build_alpha <- function(input, start_date = as.Date("2000-01-01"), end_date = today()) {
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
    mutate(open_date = ymd(.data$open_date),
           strategy = paste(.data$strategy, .data$owner, sep = ":::"),
           size = as.numeric(size))

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
        strat_end <- end_date

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
    group_by(.data$strategy, .data$identifier, .data$asset_class, .data$owner, .data$type) %>%
    summarise() %>%
    ungroup()

  list(summary = strat_summ, actual = all_actual_size, sim = all_sim_size, trades = strategies)
}


#' Get return of futures trades
#'
#' @param portfolio tidied dataframe containing `strategy`, `identifier`
#' @param trades dataframe containing all trades
#'
#' @return tidied dataframe containing the `market_pnl` and `timing_pnl` of the trades
#' @export
#'
#' @examples
get_trade_return_futures <- function(portfolio, trades) {
  summ <- trades %>%
    group_by(.data$strategy, .data$identifier, .data$asset_class, .data$owner, .data$type) %>%
    summarise() %>%
    ungroup()

  portfolio <- portfolio %>% filter(instrument %in% filter(summ, asset_class == "Futures")$identifier)

  futures <- unique(summ$identifier)
  start_date <- min(portfolio$date)
  end_date <- max(portfolio$date)

  futures_prices <- bdh_weekday(futures, "PX_LAST", start_date, end_date)%>%
    gather("instrument", "PX_LAST", -.data$date) %>%
    filter(!is.na(.data$PX_LAST)) %>%
    left_join(
      bdp(futures, "FUT_VAL_PT") %>% rownames_to_column("instrument"),
      by = "instrument") %>%
    mutate(FUT_VAL_PT = as.numeric(.data$FUT_VAL_PT))

  # Calculate per instrument daily pnl from market movements
  market_pnl <- portfolio %>%
    left_join(futures_prices, by = c("date", "instrument")) %>%
    group_by(.data$strategy, .data$instrument) %>%
    mutate(market_pnl = .data$size*.data$FUT_VAL_PT*(.data$PX_LAST - dplyr::lag(.data$PX_LAST))) %>%
    replace_na(list(market_pnl=0)) %>%
    as_tibble()

  # Calculate timing pnl
  timing_pnl <- trades %>% left_join(futures_prices, by = c("open_date" = "date", "identifier" = "instrument")) %>%
    group_by(.data$strategy, .data$identifier) %>%
    mutate(timing_pnl = .data$size * .data$FUT_VAL_PT*(.data$PX_LAST - .data$price)) %>%
    replace_na(list(timing_pnl=0)) %>%
    as_tibble()

  total_pnl <- market_pnl %>%
    left_join(
      select(timing_pnl, .data$strategy, .data$open_date, .data$timing_pnl, .data$identifier),
      by = c("date"="open_date", "strategy", "instrument" = "identifier")) %>%
    replace_na(list(timing_pnl = 0)) %>%
    select(.data$strategy, .data$date, .data$instrument, .data$market_pnl, .data$timing_pnl) %>%
    gather("pnl_type", "pnl", -.data$strategy, -.data$date, -.data$instrument)

  total_pnl %>%
    group_by(.data$strategy, .data$date, .data$pnl_type) %>%
    summarise(pnl = sum(.data$pnl, na.rm = T))  %>%
    ungroup() %>%
    group_by(.data$strategy, .data$pnl_type) %>%
    mutate(cum_pnl = cumsum(.data$pnl)) %>%
    ungroup
}

#' Get return of bonds trades
#'
#' @param portfolio tidied dataframe containing `strategy`, `identifier`
#' @param trades dataframe containing all trades
#'
#' @return tidied dataframe containing the `market_pnl` and `timing_pnl` of the trades
#' @export
#'
#' @examples
get_trade_return_bonds <- function(portfolio, trades) {
  summ <- trades %>%
    group_by(.data$strategy, .data$identifier, .data$asset_class, .data$owner, .data$type) %>%
    summarise() %>%
    ungroup()

  start_date <- min(portfolio$date)
  end_date <- max(portfolio$date)

  bonds <- filter(summ, .data$asset_class == "Bonds")$identifier

  portfolio <- filter(portfolio, .data$instrument %in% bonds)

  if (length(bonds) == 1) {
    bond_prices <- bdh_weekday(bonds, c("PX_LAST", "IDX_RATIO"), start_date, end_date) %>%
      mutate(instrument = bonds)
  } else {
    bond_prices <- bdh_weekday(bonds, c("PX_LAST", "IDX_RATIO"), start_date, end_date) %>%
      bind_rows(.id = "instrument")
  }

  bond_prices  <- bond_prices  %>%
    left_join(
      bdp(bonds, "CPN") %>% rownames_to_column("instrument"),
      by = "instrument") %>%
    replace_na(list(IDX_RATIO = 1))

  pnl <- bond_prices  %>% group_by(.data$instrument) %>%
    mutate(day_count = as.numeric(.data$date - dplyr::lag(.data$date)),
           px_chg = (.data$PX_LAST - dplyr::lag(.data$PX_LAST)) * .data$IDX_RATIO / 100,
           accrued_interest = .data$day_count / 365 * .data$CPN/100 * ifelse(is.na(.data$PX_LAST), NA, 1)) %>%
    replace_na(list(px_chg = 0, accrued_interest = 0)) %>%
    mutate(pnl = .data$px_chg + .data$accrued_interest)

  market_pnl <- portfolio %>% left_join(bond_prices , by = c("date", "instrument")) %>%
    mutate(day_count = as.numeric(.data$date - dplyr::lag(.data$date)),
           px_chg = (.data$PX_LAST - dplyr::lag(.data$PX_LAST)) * .data$IDX_RATIO / 100,
           accrued_interest = .data$day_count / 365 * .data$CPN/100 * ifelse(is.na(.data$PX_LAST), NA, 1)) %>%
    replace_na(list(px_chg = 0, accrued_interest = 0)) %>%
    mutate(market_pnl = .data$size * (.data$px_chg + .data$accrued_interest)) %>%
    select(date, .data$strategy, .data$instrument, .data$size, .data$market_pnl)

  timing_pnl <- filter(trades, .data$asset_class == "Bonds") %>%
    left_join(bond_prices, by = c("open_date" = "date", "identifier" = "instrument")) %>%
    group_by(.data$strategy, .data$identifier) %>%
    mutate(timing_pnl = .data$size * (.data$PX_LAST - .data$price)/100) %>%
    replace_na(list(timing_pnl=0)) %>%
    as_tibble()

  total_pnl <- market_pnl %>%
    left_join(
      select(timing_pnl, .data$strategy, .data$open_date, .data$timing_pnl, .data$identifier),
      by = c("date"="open_date", "strategy", "instrument" = "identifier")) %>%
    replace_na(list(timing_pnl = 0)) %>%
    select(.data$strategy, .data$date, .data$instrument, .data$market_pnl, .data$timing_pnl) %>%
    gather("pnl_type", "pnl", -.data$strategy, -.data$date, -.data$instrument)

  total_pnl %>%
    group_by(.data$strategy, .data$date, .data$pnl_type) %>%
    summarise(pnl = sum(.data$pnl, na.rm = T)) %>%
    ungroup() %>%
    group_by(.data$strategy, .data$pnl_type) %>%
    mutate(cum_pnl = cumsum(.data$pnl)) %>%
    ungroup
}




if (!interactive()) {
  alpha <- build_alpha(file.path("data2", "trademaster.csv"), start_date = as.Date("2018-01-01"))

  pnl_futures <- get_trade_return_futures(alpha$actual, alpha$trades) %>%
    mutate(asset_class = "Futures")
  pnl_bonds <- get_trade_return_bonds(alpha$actual, alpha$trades) %>%
    mutate(asset_class = "Bonds")

  pnl <- bind_rows(pnl_futures, pnl_bonds) %>%
    group_by(strategy, date, pnl_type) %>%
    summarise(pnl = sum(pnl, na.rm = T),
              cum_pnl = sum(cum_pnl, na.rm = T))

  date_limits <- alpha$actual %>% filter(size != 0) %>%
    group_by(strategy) %>%
    summarise(min_date = min(date), max_date = max(date))

  # Plot by strategy
  pnl %>%
    group_by(strategy, date) %>%
    summarise(cum_pnl = sum(.data$cum_pnl, na.rm =T)) %>%
    ungroup() %>%
    ggplot(aes(x = date, y= cum_pnl/1e6)) +
    geom_area(aes(fill = strategy)) +
    stat_summary(fun.y = sum, geom = "line", size = 1) +
    theme(axis.title.x = element_blank(), legend.position = "bottom")

  # Plot by market vs timing
  pnl %>%
    group_by(date, pnl_type) %>%
    summarise(cum_pnl = sum(.data$cum_pnl, na.rm = T)) %>%
    ungroup %>%
    ggplot(aes(x = date, y=cum_pnl / 1e6)) +
    geom_area(aes(fill = pnl_type)) +
    stat_summary(fun.y = sum, geom = "line", size = 1) +
    theme(axis.title.x = element_blank(), legend.position = "bottom")

  # Cumulative Pnl by strategy and type
  pnl %>%
    left_join(date_limits, by = "strategy") %>%
    filter(date < max_date, date > min_date) %>%
    ggplot(aes(x = date, y = cum_pnl / 1e3)) +
    geom_area(aes(fill = pnl_type)) +
    stat_summary(fun.y = sum, geom = "line", size = 0.5) +
    facet_wrap(~strategy, ncol = 2, scales = "free_x") +
    theme(legend.position = "bottom") +
    labs(title = "Cumulative pnl")

  # Pnl by strategy and type
  pnl %>%
    left_join(date_limits, by = "strategy") %>%
    filter(date < max_date, date > min_date) %>%
    ggplot(aes(x = date, y = pnl / 1e6)) +
    geom_col(aes(fill = pnl_type)) +
    facet_wrap(~strategy, ncol = 2, scales = "free") +
    theme(legend.position = "bottom") +
    labs(title = "Non-cumulative pnl")


}
