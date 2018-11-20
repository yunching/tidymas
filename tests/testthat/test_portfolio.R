context("Portfolio Analytics")

library(tidymas)

test_that("NA voids results",{
  expect_equal(z_score(c(1:10, NA)), as.numeric(rep(NA, 11)))
})
