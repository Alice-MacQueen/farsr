library(testthat)
library(farsr)

test_that("fars_read works", {
  expect_error(fars_read(filename = "file_does_not_exist.txt"))
})

test_that("fars_read_years works", {
  expect_error(fars_read_years(years = 14))
})
