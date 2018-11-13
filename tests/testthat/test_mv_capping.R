context("Market value capping")

library(tidymas)
library(purrr)
data(mvs)
data(caps)
data(mkt_capping_hist_cap)
data(mkt_capping_hist_mv)
data(mkt_capping_hist_wts)

test_that("Expect errors",{
  #Check capping more than 100%
  expect_error(market_capping(1:10, rep(100, 10)), "There should not be any weights > 100% in capped_wts.")
  #Check capping < 0%
  expect_error(market_capping(1:10, rep(-100, 10)), "There should not be any negative weights in capped_wts.")
  #Check mv and caps of different lengths
  expect_error(market_capping(1:10, rep(1, 9)), "mv and capped_mv_wts should be of same length.")
})

test_that("3 names, no capping",{
  expect_equal(market_capping(c(1:3), rep(1,3))[,"capped_mv_wts"], c(1/6, 1/3, 1/2))
})

test_that("",{
  expect_equal(market_capping(rep(25, 4), c(0.02, 1, 1, 1))[,"capped_mv_wts"], c(0.02, 0.32666667, 0.32666667, 0.32666667))
})

test_that("10 names, no capping",{
  expect_equal(market_capping(1:10, rep(1, 10))[,"capped_mv_wts"], 1:10/55)
})

test_that("3 names, 1 capped",{
  expect_equal(market_capping(c(1:3), c(0.02, 1, 1))[,"capped_mv_wts"], c(0.02, 0.392, 0.588))
})

test_that("3 names, 2 capped",{
  expect_equal(market_capping(c(1:3), c(0.02, 0.02, 1))[,"capped_mv_wts"], c(0.02, 0.02, 0.96))
})

test_that("Sep 2018",{
  expect_equal(
    market_capping(mvs, caps)[,"capped_mv_wts"],
    c(
      0.01892155,
      0.02197923,
      0.01828742,
      0.09342356,
      0.06184465,
      0.05000000,
      0.15000000,
      0.01856537,
      0.02895431,
      0.05297996,
      0.08601833,
      0.39902561
    )
  )
})

#calculate difference between new function's wts versus history
new_capped_wts <-purrr::map2(mkt_capping_hist_mv, mkt_capping_hist_cap, tidymas::market_capping) %>%
  purrr::map("capped_mv_wts") %>%
  data.frame()
sum_of_abs_diff_in_rounded_wts <- sum(abs(round(new_capped_wts - mkt_capping_hist_wts, 3)))

test_that("Check calculations based on historical mvs line up with historical capped wts",{
  expect_lte(sum_of_abs_diff_in_rounded_wts, 0.005)
})




