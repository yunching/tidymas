context("Active Risks")

library(tidymas)

date_removed_df <- date_df <- data.frame(date = c(as.Date("2018-01-01"), as.Date("2018-01-02"), as.Date("2018-01-03")), data1 = c(1,2,3), data2 = c(4,5,6))
rownames(date_removed_df) <- date_df$date
date_removed_df$date <- NULL

test_that("date_removed", expect_equal(date_removed_df, remove_date(date_df)))



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
