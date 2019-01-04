context("Build strategies")

data(demo_strategies)

# Test inputs

test_that("Accepts dataframe input", {
  expect_error(build_strategies(demo_strategies), NA)
})

test_that("Accepts character input but gives error when file cannot be found",
          expect_error(build_strategies("does_not_exist.csv")))

test_that("Gives error when multiple strategy types for the same strategy", {
  expect_error(build_strategies(
    demo_strategies %>% mutate(type = ifelse(.data$strategy == "L_Nikkei_eg_chgsize" & .data$open_date == "20180404", "FX", .data$type))
  ))
})

test_that("Gives error when invalid govt/ilb ticker", {
  expect_error(build_strategies(demo_strategies %>% mutate(identifier = ifelse(.data$identifier == "us_govt_10y", "us_gov_10y", .data$identifier))))
  expect_error(build_strategies(demo_strategies %>% mutate(identifier = ifelse(.data$identifier == "us_ilb_5y", "is_ilb_5y", .data$identifier))))

})

test_that("Error when invalid currency", {
  expect_error(build_strategies(demo_strategies %>% mutate(identifier = ifelse(.data$identifier == "eurchf", "euchf", .data$identifier))))
  expect_error(build_strategies(demo_strategies %>% mutate(identifier = ifelse(.data$identifier == "eurchf", "euechf", .data$identifier))))
  expect_error(build_strategies(demo_strategies %>% mutate(identifier = ifelse(.data$identifier == "eurchf", "usdcap", .data$identifier))))
})

test_that("Error when invalid cds", {
  expect_error(build_strategies(demo_strategies %>% mutate(identifier = ifelse(.data$identifier == "cdx_us_ig", "cdx_am_ig", .data$identifier))))
})

test_that("Warn when size_type not expected", {
  expect_error(build_strategies(demo_strategies %>% mutate(size_type = ifelse(.data$strategy == "L_US_BreakE_10y", "abc", .data$size_type))))
  expect_warning(build_strategies(demo_strategies %>% mutate(size_type = ifelse(.data$asset_class == "equity", "months", .data$size_type))))
  expect_warning(build_strategies(demo_strategies %>% mutate(size_type = ifelse(.data$asset_class == "fx", "months", .data$size_type))))
  expect_warning(build_strategies(demo_strategies %>% mutate(size_type = ifelse(.data$asset_class == "cds", "percent", .data$size_type))))
  expect_warning(build_strategies(demo_strategies %>% mutate(size_type = ifelse(.data$asset_class == "fut", "percent", .data$size_type))))
  expect_warning(build_strategies(demo_strategies %>% mutate(size_type = ifelse(.data$asset_class == "govt", "percent", .data$size_type))))
  expect_warning(build_strategies(demo_strategies %>% mutate(size_type = ifelse(.data$asset_class == "ilb", "percent", .data$size_type))))
})

test_that("Warn when multiple lines with exact same entry, possible error in entry", {
  expect_warning(build_strategies(rbind(demo_strategies, demo_strategies[c(1,2),])))
})

# Test outputs

test_that("output contains summary, actual and sim", {
  expect_true(all(c("summary", "actual", "sim") %in% names(build_strategies(demo_strategies))))
})

test_that("output actual sim has correct columns", {
  expect_true(all(c("date", "strategy", "instrument", "size") %in% names(build_strategies(demo_strategies)$actual)))
  expect_true(all(c("date", "strategy", "instrument", "size") %in% names(build_strategies(demo_strategies)$sim)))
})

test_that("all strategies are present in sim and actual", {
  expect_true(all(unique(paste(demo_strategies$strategy, demo_strategies$owner, sep = ":::"))
                  %in% build_strategies(demo_strategies)$actual$strategy))
  expect_true(all(unique(paste(demo_strategies$strategy, demo_strategies$owner, sep = ":::"))
                  %in% build_strategies(demo_strategies)$sim$strategy))
})

test_that("filtered dates are of correct range and skips weekends", {
  expect_equal(build_strategies(demo_strategies, start_date = as.Date("2018-01-01"), end_date = as.Date("2018-01-10"))[[2]]$date %>% unique,
               as.Date(c("2018-01-01", "2018-01-02", "2018-01-03", "2018-01-04", "2018-01-05", "2018-01-08", "2018-01-09", "2018-01-10")))
})

# Test actual sizes

test_output <- build_strategies(demo_strategies, start_date = as.Date("2015-01-01"), end_date = as.Date("2018-12-31"))
actual <- test_output$actual
sim <- test_output$sim
summ <- test_output$summary

test_that("actual sizes are correct for Long Nikkei, where size of strategy change over time", {
  expect_true(all(dplyr::filter(actual, strategy == "L_Nikkei_eg_chgsize:::ben", instrument == "nikkei", date > as.Date("2018-01-15") & date <= as.Date("2018-04-04"))$size == 0.23))
  expect_true(all(dplyr::filter(actual, strategy == "L_Nikkei_eg_chgsize:::ben", instrument == "nikkei", date > as.Date("2018-04-04"))$size == 0.5))

  expect_true(all(dplyr::filter(actual, strategy == "L_Nikkei_eg_chgsize:::ben", instrument == "nikkei", !(date > as.Date("2018-01-15")))$size == 0))
})

test_that("actual sizes are correct for long short positions measured in percent", {
  expect_true(all(dplyr::filter(actual, strategy == "L_STOXX_S_FTSE:::eve", instrument == "eurostoxx",  date > as.Date("2015-12-03") & date <= as.Date("2016-01-04"))$size == 0.5))
  expect_true(all(dplyr::filter(actual, strategy == "L_STOXX_S_FTSE:::eve", instrument == "ftse",  date > as.Date("2015-12-03") & date <= as.Date("2016-01-04"))$size == -0.5))

  expect_true(all(dplyr::filter(actual, strategy == "L_STOXX_S_FTSE:::eve", instrument == "eurostoxx", !(date > as.Date("2015-12-03") & date <= as.Date("2016-01-04")))$size == 0))
  expect_true(all(dplyr::filter(actual, strategy == "L_STOXX_S_FTSE:::eve", instrument == "ftse", !(date > as.Date("2015-12-03") & date <= as.Date("2016-01-04")))$size == 0))
})

test_that("actual sizes are correct for positions without end_date, should be assumed to be closed at end_date", {
  expect_true(all(dplyr::filter(actual, strategy == "Stpnr_Euribor_3-7:::eve", instrument == "fut_er3", date > as.Date("2018-01-05") & date <= as.Date("2018-12-31"))$size == 0.23))
  expect_true(all(dplyr::filter(actual, strategy == "Stpnr_Euribor_3-7:::eve", instrument == "fut_er7", date > as.Date("2018-01-05") & date <= as.Date("2018-12-31"))$size == -0.23))

  expect_true(all(dplyr::filter(actual, strategy == "Stpnr_Euribor_3-7:::eve", instrument == "fut_er3", !(date > as.Date("2018-01-05") & date <= as.Date("2018-12-31")))$size == 0))
  expect_true(all(dplyr::filter(actual, strategy == "Stpnr_Euribor_3-7:::eve", instrument == "fut_er7", !(date > as.Date("2018-01-05") & date <= as.Date("2018-12-31")))$size == 0))
})

test_that("actual sizes are correct for long short positions measured in months", {
  expect_true(all(dplyr::filter(actual, strategy == "Stpnr_JP_10s30s:::ben", instrument == "japan_govt_10y", date > as.Date("2016-08-17") & date <= as.Date("2016-11-24"))$size == 0.38))
  expect_true(all(dplyr::filter(actual, strategy == "Stpnr_JP_10s30s:::ben", instrument == "japan_govt_30y", date > as.Date("2016-08-17") & date <= as.Date("2016-11-24"))$size == -0.38))

  expect_true(all(dplyr::filter(actual, strategy == "Stpnr_JP_10s30s:::ben", instrument == "japan_govt_10y", !(date > as.Date("2016-08-17") & date <= as.Date("2016-11-24")))$size == 0))
  expect_true(all(dplyr::filter(actual, strategy == "Stpnr_JP_10s30s:::ben", instrument == "japan_govt_30y", !(date > as.Date("2016-08-17") & date <= as.Date("2016-11-24")))$size == 0))
})

test_that("actual sizes are correct for currency positions", {
  expect_true(all(dplyr::filter(actual, strategy == "L_EUR:::kev", instrument == "eurchf", date > as.Date("2017-01-05") & date <= as.Date("2018-05-08"))$size == 0.93))
  expect_true(all(dplyr::filter(actual, strategy == "L_EUR:::kev", instrument == "eurcad", date > as.Date("2017-06-05") & date <= as.Date("2018-08-10"))$size == 0.72))
  expect_true(all(dplyr::filter(actual, strategy == "L_EUR:::kev", instrument == "eurusd", date > as.Date("2018-01-03") & date <= as.Date("2018-08-10"))$size == 0.76))

  expect_true(all(dplyr::filter(actual, strategy == "L_EUR:::kev", instrument == "eurchf", !(date > as.Date("2017-01-05") & date <= as.Date("2018-05-08")))$size == 0))
  expect_true(all(dplyr::filter(actual, strategy == "L_EUR:::kev", instrument == "eurcad", !(date > as.Date("2017-06-05") & date <= as.Date("2018-08-10")))$size == 0))
  expect_true(all(dplyr:::filter(actual, strategy == "L_EUR:::kev", instrument == "eurusd", !(date > as.Date("2018-01-03") & date <= as.Date("2018-08-10")))$size == 0))
})


# Test simulated sizes

test_that("sim sizes are correct for Long Nikkei, where size of strategy change over time", {
  expect_true(all(dplyr:::filter(sim, strategy == "L_Nikkei_eg_chgsize:::ben", instrument == "nikkei", date <= as.Date("2018-04-04"))$size == 0.23))
  expect_true(all(dplyr::filter(sim, strategy == "L_Nikkei_eg_chgsize:::ben", instrument == "nikkei", date > as.Date("2018-04-04"))$size == 0.5))
})

test_that("sim sizes are correct for long short positions measured in percent", {
  expect_true(all(dplyr:::filter(sim, strategy == "L_STOXX_S_FTSE:::eve", instrument == "eurostoxx50")$size == 0.5))
  expect_true(all(dplyr:::filter(sim, strategy == "L_STOXX_S_FTSE:::eve", instrument == "ftse100")$size == -0.5))
})

test_that("sim sizes are correct for positions without end_date, should be assumed to be closed at end_date", {
  expect_true(all(dplyr:::filter(sim, strategy == "Stpnr_Euribor_3-7:::eve", instrument == "fut_er3")$size == 0.23))
  expect_true(all(dplyr:::filter(sim, strategy == "Stpnr_Euribor_3-7:::eve", instrument == "fut_er7")$size == -0.23))
})

test_that("sim sizes are correct for long short positions measured in months", {
  expect_true(all(dplyr:::filter(sim, strategy == "Stpnr_JP_10s30s:::ben", instrument == "japan_govt_10y")$size == 0.38))
  expect_true(all(dplyr:::filter(sim, strategy == "Stpnr_JP_10s30s:::ben", instrument == "japan_govt_30y")$size == -0.38))
})

test_that("sim sizes are correct for currency positions, back filled based on earliest available mix of positions", {
  expect_true(all(dplyr:::filter(sim, strategy == "L_EUR:::kev", instrument == "eurchf", date <= as.Date("2017-06-05"))$size == 0.93))
  expect_true(all(dplyr:::filter(sim, strategy == "L_EUR:::kev", instrument == "eurcad", date <= as.Date("2017-06-05"))$size == 0))
  expect_true(all(dplyr:::filter(sim, strategy == "L_EUR:::kev", instrument == "eurusd", date <= as.Date("2017-06-05"))$size == 0))
})

test_that("sim sizes are correct for currency positions, forward filled based on latest available mix", {
  expect_true(all(dplyr:::filter(sim, strategy == "L_EUR:::kev", instrument == "eurchf", date > as.Date("2018-05-08"))$size == 0))
  expect_true(all(dplyr:::filter(sim, strategy == "L_EUR:::kev", instrument == "eurcad", date > as.Date("2018-05-08"))$size == 0.72))
  expect_true(all(dplyr:::filter(sim, strategy == "L_EUR:::kev", instrument == "eurusd", date > as.Date("2018-05-08"))$size == 0.76))
})

# Test summary

test_that("summary is in output", {
  expect_false(is.null(summ))
})

test_that("summary contains required essential columns",
  expect_true(all(c("strategy", "identifier", "asset_class", "size_type") %in% names(summ)))
)

test_that("summary contains all strategies", {
  expect_true(all(unique(paste(demo_strategies$strategy, demo_strategies$owner, sep = ":::")) %in% summ$strategy))
})

