context("Test weighted and unweighted return calculations")

data(demo_strategies)
data(demo_duration)
data(demo_return)

portfolios_list <- build_strategies(demo_strategies, start_date = as.Date("2016-01-01"), end_date = as.Date("2018-06-01"))
sim <- convert_dur_size(portfolios_list$sim, portfolios_list$summary, demo_duration)

wt_return <- calc_strat_wt_return(sim, demo_return)

# Check weighted return
test_that("weighted returns are aggregated by strategy",
          expect_false("instrument" %in% names(wt_return)))

ilb_10y_size <- dplyr::filter(sim, .data$strategy == "Fltnr_US_ILB_5s10s:::jule", .data$instrument == "us_ilb_10y", .data$date == as.Date("2016-05-04"))$size
ilb_10y_return <- dplyr::filter(demo_return, .data$instrument == "us_ilb_10y", .data$date == as.Date("2016-05-04"))$return
ilb_5y_size <- dplyr::filter(sim, .data$strategy == "Fltnr_US_ILB_5s10s:::jule", .data$instrument == "us_ilb_5y", .data$date == as.Date("2016-05-04"))$size
ilb_5y_return <- dplyr::filter(demo_return, .data$instrument == "us_ilb_5y", .data$date == as.Date("2016-05-04"))$return


stoxx_size <- dplyr::filter(sim, .data$strategy == "L_STOXX_S_FTSE:::eve", .data$instrument == "eurostoxx50", .data$date == as.Date("2017-12-20"))$size
stoxx_return <- dplyr::filter(demo_return, .data$instrument == "eurostoxx50", .data$date == as.Date("2017-12-20"))$return
ftse_size <- dplyr::filter(sim, .data$strategy == "L_STOXX_S_FTSE:::eve", .data$instrument == "ftse100", .data$date == as.Date("2017-12-20"))$size
ftse_return <- dplyr::filter(demo_return, .data$instrument == "ftse100", .data$date == as.Date("2017-12-20"))$return

test_that("Weighted returns are correct", {
  expect_equal(dplyr::filter(wt_return, .data$strategy == "Fltnr_US_ILB_5s10s:::jule", .data$date == as.Date("2016-05-04"))$wt_return,
               ilb_10y_size * ilb_10y_return + ilb_5y_size *ilb_5y_return)
  expect_equal(dplyr::filter(wt_return, .data$strategy == "L_STOXX_S_FTSE:::eve", .data$date == as.Date("2017-12-20"))$wt_return,
               stoxx_size * stoxx_return + ftse_size *ftse_return)
})


# Check headline calculations
headline <- calc_strat_headline_size(sim)

test_that("Headline size is correct", {
  expect_true(abs(dplyr::filter(headline, .data$strategy == "Fltnr_US_ILB_5s10s:::jule", .data$date == as.Date("2016-04-04"))$size
              - 0.01104138) < 0.0001)
  expect_equal(dplyr::filter(headline, .data$strategy == "L_Nikkei_eg_chgsize:::ben", .data$date == as.Date("2018-01-22"))$size,
               0.0023)
  expect_equal(dplyr::filter(headline, .data$strategy == "L_Nikkei_eg_chgsize:::ben", .data$date == as.Date("2018-04-10"))$size,
               0.005)
  expect_equal(dplyr::filter(headline, .data$strategy == "L_EUR:::kev", .data$date == as.Date("2018-05-10"))$size,
               0.0148)
})


# Check unweighted return
unwt_return <- calc_strat_unwt_return(wt_return, headline)



test_that("Unwt return is correct", {
  expect_equal(dplyr::filter(unwt_return, .data$strategy == "Fltnr_US_ILB_5s10s:::jule", .data$date == as.Date("2016-04-04"))$return,
                  dplyr::filter(wt_return, .data$strategy == "Fltnr_US_ILB_5s10s:::jule", .data$date == as.Date("2016-04-04"))$wt_return /
                    dplyr::filter(headline, .data$strategy == "Fltnr_US_ILB_5s10s:::jule", .data$date == as.Date("2016-04-04"))$size)

  expect_equal(dplyr::filter(unwt_return, .data$strategy == "L_Nikkei_eg_chgsize:::ben", .data$date == as.Date("2018-01-22"))$return,
               dplyr::filter(wt_return, .data$strategy == "L_Nikkei_eg_chgsize:::ben", .data$date == as.Date("2018-01-22"))$wt_return /
                 dplyr::filter(headline, .data$strategy == "L_Nikkei_eg_chgsize:::ben", .data$date == as.Date("2018-01-22"))$size)

  expect_equal(dplyr::filter(unwt_return, .data$strategy == "L_Nikkei_eg_chgsize:::ben", .data$date == as.Date("2018-04-10"))$return,
               dplyr::filter(wt_return, .data$strategy == "L_Nikkei_eg_chgsize:::ben", .data$date == as.Date("2018-04-10"))$wt_return /
                 dplyr::filter(headline, .data$strategy == "L_Nikkei_eg_chgsize:::ben", .data$date == as.Date("2018-04-10"))$size)

  expect_equal(dplyr::filter(unwt_return, .data$strategy == "L_EUR:::kev", .data$date == as.Date("2018-05-10"))$return,
               dplyr::filter(wt_return, .data$strategy == "L_EUR:::kev", .data$date == as.Date("2018-05-10"))$wt_return /
                 dplyr::filter(headline, .data$strategy == "L_EUR:::kev", .data$date == as.Date("2018-05-10"))$size)
})
