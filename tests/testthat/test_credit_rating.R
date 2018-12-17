context("Credit rating translations")

library(tidymas)
library(stringr)

test_that("NA", {
  expect_equal(clean_rating(NA), NA)
})

test_that("AAA",{
  expect_equal(clean_rating("AAA"), "AAA")
  expect_equal(clean_rating("AAAu"), "AAA")
  expect_equal(clean_rating("AAAu   *-"), "AAA")
  expect_equal(clean_rating("Aaa"), "AAA")
  expect_equal(clean_rating("Aaa    *-"), "AAA")
})

test_that("AA+",{
  expect_equal(clean_rating("AA+"), "AA+")
  expect_equal(clean_rating("AA+u"), "AA+")
  expect_equal(clean_rating("Aa1"), "AA+")
  expect_equal(clean_rating("Aa1    *-"), "AA+")
})

test_that("AA",{
  expect_equal(clean_rating("AAu"), "AA")
  expect_equal(clean_rating("AAu    *-"), "AA")
  expect_equal(clean_rating("AA"), "AA")
  expect_equal(clean_rating("Aa2"), "AA")
  expect_equal(clean_rating("Aa2    *-"), "AA")
  expect_equal(clean_rating("AA"), "AA")
  expect_equal(clean_rating("AA     *-"), "AA")

})

test_that("AA-",{
  expect_equal(clean_rating("AA-u"), "AA-")
  expect_equal(clean_rating("AA-    *-"), "AA-")
  expect_equal(clean_rating("Aa3"), "AA-")
  expect_equal(clean_rating("Aa3    *-"), "AA-")
  expect_equal(clean_rating("AA-"), "AA-")
  expect_equal(clean_rating("AA-    *+"), "AA-")
  expect_equal(clean_rating("AA-    *-"), "AA-")
})

test_that("A+",{
  expect_equal(clean_rating("A+"), "A+")
  expect_equal(clean_rating("A+u"), "A+")
  expect_equal(clean_rating("A1"), "A+")
  expect_equal(clean_rating("A+"), "A+")
  expect_equal(clean_rating("A+     *-"), "A+")
})
