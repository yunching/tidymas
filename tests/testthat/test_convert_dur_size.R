context("Convert duration size")

data(demo_strategies)
data(demo_duration)

portfolios_list <- build_strategies(demo_strategies, start_date = as.Date("2016-01-01"), end_date = as.Date("2018-06-01"))
sim <- convert_dur_size(portfolios_list$sim, portfolios_list$summary, demo_duration)

test_that("Function returns error when data in duration_df insufficient",
          expect_error(
            convert_dur_size(portfolios_list$actual,
                             portfolios_list$summary,
                             dplyr::filter(demo_duration, .data$date < as.Date("2017-01-01"))))
)

test_that("Strategies sized in percent are converted to decimal",
          expect_true(
  all(dplyr::filter(sim, .data$strategy == "L_STOXX_S_FTSE:::eve")$size ==
      dplyr::filter(portfolios_list$sim, .data$strategy == "L_STOXX_S_FTSE:::eve")$size/100)))

test_that("Strategies sized in months are correctly converted", {
  expect_true(all(abs(dplyr::filter(sim, .data$strategy == "Fltnr_US_ILB_5s10s:::jule",
             .data$instrument == "us_ilb_10y",
             .data$date > as.Date("2017-06-06") & .data$date < as.Date("2017-06-10"))$size -
        c(0.007864564, 0.007868472, 0.007876301)) < 0.0001))

  expect_true(all(abs(dplyr::filter(sim, .data$strategy == "Fltnr_US_ILB_5s10s:::jule",
             .data$instrument == "us_ilb_5y",
             .data$date > as.Date("2017-06-06") & .data$date < as.Date("2017-06-10"))$size -
        c(-0.01094029, -0.01094596, -0.01096301)) < 0.0001))
})

