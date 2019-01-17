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
    rename(portfolio = .data$Portfolio, owner = .data$Owner, strategy = .data$Sub.strategy, type = .data$Strategy, open_date = .data$Entry.Date, identifier = .data$Security.ID, size = .data$Amount, price = .data$Entry.Price) %>%
    mutate(identifier = str_replace(.data$identifier, " Corp$", " Govt")) %>%
    mutate(asset_class = ifelse(str_detect(.data$identifier, " (Govt|Corp)"), "Bonds", "Futures"))

  strategies <- strategies %>%
    mutate(open_date = ymd(.data$open_date),
#           strategy = paste(.data$strategy, .data$owner, sep = ":::"),
           size = as.numeric(gsub(",", "", size)))

  ## Iterate through each unique strategy
  # list_of_strat <- map(unique(strategies$strategy), function(i) {
  #   # Filter out current strategy
  #   curr_strat <- strategies %>% filter(.data$strategy == i)
  #   inst_list <- curr_strat$identifier %>% unique
  #   actual_size <- data.frame(date = gen_weekdays(start_date, end_date))
  #
  #   # Iterate through all identifiers required for this strategy
  #   for (curr_inst in inst_list) {
  #     curr_inst_df <- curr_strat %>% filter(.data$identifier == curr_inst)
  #
  #     actual_size[[curr_inst]] <- 0
  #
  #     # Set weight as actual first
  #     for (j in 1:nrow(curr_inst_df)) {
  #       # Strat start and end
  #       strat_start <- if (is.na(curr_inst_df$open_date[j])) end_date - 1 else curr_inst_df$open_date[j]
  #       strat_end <- end_date
  #
  #       actual_size[[curr_inst]] <- actual_size[[curr_inst]] +
  #         if_else(actual_size$date <= strat_end & actual_size$date > strat_start,
  #                 curr_inst_df$size[j], 0)
  #     }
  #   }
  #
  #       # Calculate simulated size
  #   sim_size <- actual_size %>%
  #     mutate(has_position = sign(rowSums(abs(.[-1])))) %>%  # Check if there's any position at the time
  #     gather("asset", "size", -.data$date, -.data$has_position) %>%  # convert into gathered dataframe
  #     mutate(size = ifelse(.data$has_position, .data$size, NA)) %>%   # if does not have any position, replace with NA
  #     spread(.data$asset, .data$size) %>%  # Convert back into original layout
  #     select(-.data$has_position) %>%
  #     fill(-.data$date, .direction = "up")  %>%
  #     fill(-.data$date, .direction = "down") # Fill up, then down to times without position, to calculate returns for simulation and correlations
  #
  #   # Mutate into tidy form
  #   actual_size <- actual_size %>%
  #     mutate(strategy = i) %>%
  #     gather("instrument", "size", -.data$date, -.data$strategy)
  #
  #   sim_size <- sim_size %>%
  #     mutate(strategy = i) %>%
  #     gather("instrument", "size", -.data$date, -.data$strategy)
  #
  #
  #   list(actual = actual_size,
  #        sim = sim_size)
  # })

  # Iterate through all unique strategies (incl portfolio, strategy, owner)
  all_actual_size <- pmap(unique(select(strategies, pf = portfolio, strat = strategy, own = owner)),
                        function(pf, strat, own) {
                          # Filter out trades in current strategy
                          curr_strat <- strategies %>% dplyr::filter(.data$strategy == strat, .data$portfolio == pf, .data$owner == own)
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

                          # Mutate into tidy form
                          actual_size <- actual_size %>%
                            gather("instrument", "size", -.data$date) %>%
                            mutate(portfolio = pf, strategy = strat, owner = own)

                          actual_size
                        }) %>%
    bind_rows() %>%
    filter(.data$size != 0) %>%
    as.tibble()


  # all_actual_size <- map(list_of_strat, ~.$actual) %>%
  #   reduce(rbind) %>%
  #   as.tibble
  #
  # all_sim_size <- map(list_of_strat, ~.$sim) %>%
  #   reduce(rbind) %>%
  #   as.tibble

  # Summarise strategies (for use in size conversions later)
  strat_summ <- strategies %>%
    group_by(.data$strategy, .data$portfolio, .data$owner, .data$identifier, .data$asset_class, .data$owner, .data$type) %>%
    summarise() %>%
    ungroup()

  list(summary = strat_summ, actual = all_actual_size, trades = strategies)
}


#' Get specifications for futures (include adjustment for expired tickers)
#'
#' @param tickers character vector containing bloomberg tickers
#' @param fields character vector containing required fields
#' @param ... parameters to be passed to `bdp`` formula e.g. options and overrides, see `bdp`
#'
#' @return
#' @export
#'
#' @examples
#' \donttest{
#' bdp_fut(c("RX1 Comdty", "RXH6 Comdty"), c("FUT_VAL_PT", "CRNCY"))
#' }
bdp_fut <- function(tickers, fields, ...) {
  futures_dat1 <- bdp(tickers, fields) %>%
    rownames_to_column("instrument") %>%
    replace(is.na(.), "")

  renamed_futures <- futures_dat1 %>%
    unite("all", -.data$instrument, sep = "") %>%
    filter(.data$all == "") %>%
    mutate(renamed_inst = str_replace(.data$instrument, "(?=[0-9]+ )", "1")) %>%
    select(.data$instrument, .data$renamed_inst)

  futures_dat2 <- NULL
  if (nrow(renamed_futures) > 0) {
    futures_dat2 <- bdp(renamed_futures$renamed_inst, fields) %>%
      rownames_to_column("renamed_inst") %>%
      left_join(renamed_futures, by = "renamed_inst")
  }

  bind_rows(futures_dat1 %>%
              filter(!.data$instrument %in% renamed_futures$instrument) %>%
              mutate(renamed_inst = NA),
            futures_dat2) %>%
    mutate(renamed_inst = ifelse(is.na(.data$renamed_inst), .data$instrument, .data$renamed_inst))
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
get_trade_return_futures <- function(portfolio, trades, curr) {
  summ <- trades %>%
    group_by(.data$strategy, .data$identifier, .data$asset_class, .data$owner, .data$type) %>%
    summarise() %>%
    ungroup()

  portfolio <- portfolio %>% filter(instrument %in% filter(summ, asset_class == "Futures")$identifier)

  futures <- unique(portfolio$instrument)
  start_date <- min(portfolio$date - 5)
  end_date <- max(portfolio$date)

  # Download contract specifications
  futures_specs <- bdp_fut(futures, c("FUT_VAL_PT", "CRNCY"))

  # Download currency pairs
  req_curr <- futures_specs['CRNCY'] %>%
    unique %>%
    filter(.data$CRNCY != curr) %>%
    mutate(ticker = paste0(.data$CRNCY, curr, " Curncy")) %>%
    rename(name = .data$CRNCY)

  fx_prices <- bdh_weekday(req_curr, "PX_LAST", start_date, end_date) %>%
    mutate(!!curr := 1) %>%
    gather("fx", "fx_price", -.data$date) %>%
    mutate(fx_price = ifelse(fx == "JPY", fx_price / 100, fx_price))

  # Download prices based on renamed futures names
  futures_prices <- bdh_weekday(futures_specs$renamed_inst, "PX_LAST", start_date, end_date)%>%
    gather("instrument", "PX_LAST", -.data$date) %>%
#    filter(!is.na(.data$PX_LAST)) %>%
    left_join(
      bdp(futures, c("FUT_VAL_PT", "CRNCY")) %>% rownames_to_column("instrument"),
      by = "instrument") %>%
    mutate(FUT_VAL_PT = as.numeric(.data$FUT_VAL_PT)) %>%
    rename(renamed_inst = .data$instrument, fx = .data$CRNCY) %>%
    left_join(select(futures_specs, .data$instrument, .data$renamed_inst), by = "renamed_inst") %>%
    select(-.data$renamed_inst) %>%
    group_by(instrument) %>%
    mutate(return = .data$PX_LAST - dplyr::lag(.data$PX_LAST))

  # Futures FX returns is only the daily pnl in local currency terms multiplied by today's FX rates, as futures are traded on margin
  # Hence there is no exposure on the contract value of the contracts.

  # Calculate per instrument daily pnl from market movements
  market_pnl <- portfolio %>%
    left_join(futures_prices, by = c("date", "instrument")) %>%
    left_join(fx_prices, by = c("date", "fx")) %>%
    group_by(.data$strategy, .data$instrument) %>%
    mutate(market_pnl = .data$fx_price * .data$size*.data$FUT_VAL_PT*(.data$return)) %>%
    replace_na(list(market_pnl=0)) %>%
    as_tibble()

  # Calculate timing pnl
  timing_pnl <- trades %>%
    left_join(futures_prices, by = c("open_date" = "date", "identifier" = "instrument")) %>%
    left_join(fx_prices, by = c("open_date" = "date", "fx")) %>%
    group_by(.data$strategy, .data$identifier) %>%
    mutate(timing_pnl = .data$fx_price * .data$size * .data$FUT_VAL_PT*(.data$PX_LAST - .data$price)) %>%
    replace_na(list(timing_pnl=0)) %>%
    as_tibble()

  total_pnl <- market_pnl %>%
    select(.data$date, .data$instrument, .data$size, .data$portfolio, .data$strategy, .data$owner, .data$market_pnl) %>%
    left_join(
      select(timing_pnl, .data$strategy, .data$open_date, .data$timing_pnl, .data$identifier),
      by = c("date"="open_date", "strategy", "instrument" = "identifier")) %>%
    replace_na(list(timing_pnl = 0)) %>%
    select(.data$strategy, .data$portfolio, .data$owner, .data$date, .data$instrument, .data$market_pnl, .data$timing_pnl) %>%
    gather("pnl_type", "pnl", -.data$strategy, -.data$date, -.data$instrument, -.data$portfolio, -.data$owner)

  total_pnl %>%
    group_by(.data$strategy, .data$portfolio, .data$owner, .data$date, .data$pnl_type) %>%
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
get_trade_return_bonds <- function(portfolio, trades, curr) {
  summ <- trades %>%
    group_by(.data$strategy, .data$identifier, .data$asset_class, .data$owner, .data$type) %>%
    summarise() %>%
    ungroup()

  start_date <- min(portfolio$date - 5)
  end_date <- max(portfolio$date)

  bonds <- filter(summ, .data$asset_class == "Bonds")$identifier %>% unique

  portfolio <- filter(portfolio, .data$instrument %in% bonds)

  if (length(bonds) == 1) {
    bond_prices_only <- bdh_weekday(bonds, c("PX_LAST", "IDX_RATIO"), start_date, end_date) %>%
      mutate(instrument = bonds)
  } else {
    bond_prices_only <- bdh_weekday(bonds, c("PX_LAST", "IDX_RATIO"), start_date, end_date) %>%
      bind_rows(.id = "instrument")
  }

  bond_prices  <- bond_prices_only  %>%
    left_join(
      bdp(bonds, c("CPN", "CRNCY")) %>% rownames_to_column("instrument"),
      by = "instrument") %>%
    rename(fx = CRNCY) %>%
    replace_na(list(IDX_RATIO = 1))

  req_curr <- bond_prices['fx'] %>%
    unique %>%
    filter(fx != curr) %>%
    mutate(ticker = paste0(.data$fx, curr, " Curncy")) %>%
    rename(name = fx)

  fx_prices <- bdh_weekday(req_curr, "PX_LAST", start_date, end_date) %>%
    mutate(!!curr := 1) %>%
    gather("fx", "fx_rate", -.data$date) %>%
    mutate(fx_rate = ifelse(fx == "JPY", fx_rate / 100, fx_rate))

  # Add one extra earlier day where all positions are zero, for proper PNL calculations
  one_day <- filter(bond_prices, .data$date < min(portfolio$date))$date %>% unique %>% max()

  portfolio <- rbind(portfolio %>% filter(.data$date == min(.data$date)) %>% mutate(size = 0, date = one_day),
        portfolio)

  market_pnl <- portfolio %>%
    left_join(bond_prices , by = c("date", "instrument")) %>%
    left_join(fx_prices, by = c("date", "fx")) %>%
    group_by(.data$strategy, .data$owner, .data$portfolio, .data$instrument) %>%
      mutate(day_count = as.numeric(.data$date - dplyr::lag(.data$date)),
             px_chg = .data$fx_rate * (.data$PX_LAST - dplyr::lag(.data$PX_LAST)) * .data$IDX_RATIO / 100,
             accrued_interest = .data$day_count / 365 * .data$CPN/100 * ifelse(is.na(.data$PX_LAST), NA, 1) * .data$fx_rate,
             fx_ret = ((.data$PX_LAST * .data$fx_rate - dplyr::lag(.data$PX_LAST) * dplyr::lag(.data$fx_rate)) * .data$IDX_RATIO / 100) - px_chg) %>%

    replace_na(list(px_chg = 0, accrued_interest = 0, fx_ret = 0)) %>%
    mutate(market_pnl = .data$size * (.data$px_chg + .data$accrued_interest),
           fx_pnl = .data$size * .data$fx_ret) %>%
    select(date, .data$strategy, .data$portfolio, .data$owner, .data$instrument, .data$size, .data$market_pnl, .data$fx_pnl) %>%
    ungroup()

  timing_pnl <- filter(trades, .data$asset_class == "Bonds") %>%
    left_join(bond_prices, by = c("open_date" = "date", "identifier" = "instrument")) %>%
    left_join(fx_prices, by = c("open_date" = "date", "fx")) %>%
    group_by(.data$strategy, .data$identifier) %>%
    mutate(timing_pnl = .data$size * (.data$PX_LAST - .data$price)/100 * .data$fx_rate) %>%
    replace_na(list(timing_pnl=0)) %>%
    as_tibble()

  total_pnl <- market_pnl %>%
    left_join(
      select(timing_pnl, .data$strategy, .data$open_date, .data$timing_pnl, .data$identifier),
      by = c("date"="open_date", "strategy", "instrument" = "identifier")) %>%
    replace_na(list(timing_pnl = 0)) %>%
    select(.data$strategy, .data$portfolio, .data$owner, .data$date, .data$instrument, .data$market_pnl, .data$timing_pnl, .data$fx_pnl) %>%
    gather("pnl_type", "pnl", -.data$strategy, -.data$date, -.data$instrument, -.data$portfolio, -.data$owner)

  total_pnl %>%
    group_by(.data$strategy, .data$portfolio, .data$owner, .data$date, .data$pnl_type) %>%
    summarise(pnl = sum(.data$pnl, na.rm = T)) %>%
    ungroup() %>%
    group_by(.data$strategy, .data$pnl_type) %>%
    mutate(cum_pnl = cumsum(.data$pnl)) %>%
    ungroup %>%
    filter(.data$date != one_day)
}
