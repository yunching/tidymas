context("Get and Check tickers")

library(tidymas)

test_that("get_tickers return results for govt, fut, equity, cds, fx", {
  expect_error(get_tickers("fx"), NA)
  expect_error(get_tickers("fut"), NA)
  expect_error(get_tickers("equity"), NA)
  expect_error(get_tickers("cds"), NA)
  expect_error(get_tickers("fx"), NA)
})

test_that("fx has usd, eur, jpy, gbp", {
  expect_true("usd" %in% get_tickers("fx")$currency)
  expect_true("eur" %in% get_tickers("fx")$currency)
  expect_true("jpy" %in% get_tickers("fx")$currency)
  expect_true("gbp" %in% get_tickers("fx")$currency)
})

test_that("tickers match correctly for FX", {
  expect_match(dplyr::filter(get_tickers("fx"), currency == "eur")$ticker, "EUDR[12]T Index")
  expect_match(dplyr::filter(get_tickers("fx"), currency == "usd")$ticker, "USDR[12]T Index")
  expect_match(dplyr::filter(get_tickers("fx"), currency == "jpy")$ticker, "JYDR[12]T Index")
  expect_match(dplyr::filter(get_tickers("fx"), currency == "gbp")$ticker, "BPDR[12]T Index")
})

test_that("price tickers match correctly for futures", {
  expect_match(dplyr::filter(get_tickers("fut"), identifier == "fut_er1")$ticker_price, "ER1 Comdty")
  expect_match(dplyr::filter(get_tickers("fut"), identifier == "fut_rx1")$ticker_price, "RX1 Comdty")
  expect_match(dplyr::filter(get_tickers("fut"), identifier == "fut_ty1")$ticker_price, "TY1 Comdty")
  expect_match(dplyr::filter(get_tickers("fut"), identifier == "fut_jb1")$ticker_price, "JB1 Comdty")
})

test_that("duration tickers match correctly for futures", {
  expect_match(dplyr::filter(get_tickers("fut"), identifier == "fut_er1")$ticker_duration, "ER1 Comdty")
  expect_match(dplyr::filter(get_tickers("fut"), identifier == "fut_rx1")$ticker_duration, "RX1 Comdty")
  expect_match(dplyr::filter(get_tickers("fut"), identifier == "fut_ty1")$ticker_duration, "TY1 Comdty")
  expect_match(dplyr::filter(get_tickers("fut"), identifier == "fut_jb1")$ticker_duration, "JB1 Comdty")
})

test_that("price tickers match correctly for govt", {
  expect_match(dplyr::filter(get_tickers("govt"), identifier == "us_govt_10y")$ticker_price, "BEUSG4 Index")
  expect_match(dplyr::filter(get_tickers("govt"), identifier == "germany_govt_5y")$ticker_price, "BEGR37 Index")
  expect_match(dplyr::filter(get_tickers("govt"), identifier == "us_ilb_5y")$ticker_price, "BCITCT Index")
  expect_match(dplyr::filter(get_tickers("govt"), identifier == "france_ilb_2y")$ticker_price, "BCIF4T Index")
})

test_that("duration tickers match correctly for govt", {
  expect_match(dplyr::filter(get_tickers("govt"), identifier == "us_govt_10y")$ticker_duration, "BEUSG4 Index")
  expect_match(dplyr::filter(get_tickers("govt"), identifier == "germany_govt_5y")$ticker_duration, "BEGR37 Index")
  expect_match(dplyr::filter(get_tickers("govt"), identifier == "us_ilb_5y")$ticker_duration, "BCITCT Index")
  expect_match(dplyr::filter(get_tickers("govt"), identifier == "france_ilb_2y")$ticker_duration, "BCIF4T Index")
})

# SAMPLES

# test_that("AAA",{
#   expect_equal(clean_rating("AAA"), "AAA")
#   expect_equal(clean_rating("AAAu"), "AAA")
#   expect_equal(clean_rating("AAAu   *-"), "AAA")
#   expect_equal(clean_rating("Aaa"), "AAA")
#   expect_equal(clean_rating("Aaa    *-"), "AAA")
# })
#
# test_that("Expect errors",{
#   #Check capping more than 100%
#   expect_error(market_capping(1:10, rep(100, 10)), "There should not be any weights > 100% in capped_wts.")
#   #Check capping < 0%
#   expect_error(market_capping(1:10, rep(-100, 10)), "There should not be any negative weights in capped_wts.")
#   #Check mv and caps of different lengths
#   expect_error(market_capping(1:10, rep(1, 9)), "mv and capped_mv_wts should be of same length.")
# })
#
# test_that("3 names, no capping",{
#   expect_equal(market_capping(c(1:3), rep(1,3))[,"capped_mv_wts"], c(1/6, 1/3, 1/2))
# })
