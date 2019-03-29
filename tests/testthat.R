library(testthat)
library(farsr)

test_check("farsr")

test_that("fars_read_years works", {
  df1 <- fars_read_years(years = 2014)
  expect_equivalent(nrow(df1[[1]]), 30056)
})

test_that("fars_read_years works", {
  expect_error(fars_read_years(years = 14))
})
