context("Pivot points")

library(tidymas)

data(swat_data)
df <- swat_data
df_rx <- swat_data %>% dplyr::filter(Ticker == "RX1 Comdty") %>% dplyr::select(-Ticker)

test_that("Expect errors for invalid input",{
  # test if not dataframe
  expect_error(en.pivot(c(1,2,3)))
  expect_error(en.pivot(df), 'Price Data Series must be Date-Open-High-Low-Close or Date-High-Low-Close')
  expect_error(en.pivot(df_rx), NA)
})

test_that("Computation methodology is correct", {
  test_input <- df_rx %>% filter(date == as.Date("2014-05-26"))
  correct_output <- c(146.25, 146.41, 146.77, 146.05, 145.89, 145.69, 145.33)
  expect_equal(en.pivot(test_input) %>% dplyr::select(-date) %>% unlist %>% as.numeric, correct_output)
})
