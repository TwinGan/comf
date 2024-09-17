library(testthat)

test_that("Test SI units with a scalar outdoor air temperature", {
  result <- calcclo_tout(27)
  expect_equal(result, 0.46)
})

test_that("Test SI units with an array of outdoor air temperatures", {
  result <- calcclo_tout(c(27, 25))
  expect_equal(result, c(0.46, 0.47))
})

test_that("Test IP units with a scalar outdoor air temperature", {
  result <- calcclo_tout(80.6, units = "IP")
  expect_equal(result, 0.46)
})

test_that("Test IP units with an array of outdoor air temperatures", {
  result <- calcclo_tout(c(80.6, 77), units = "IP")
  expect_equal(result, c(0.46, 0.47))
})

test_that("Test edge cases of the piecewise function", {
  result <- calcclo_tout(c(4, 2, 0))
  expect_equal(result, c(0.67, 0.75, 0.82))

  result <- calcclo_tout(c(-4, -6))
  expect_equal(result, c(0.96, 1.00))
})

test_that("Test invalid units", {
  expect_error(calcclo_tout(27, units = "invalid"), "Invalid unit")
})

test_that("Test invalid type for tout", {
  expect_error(calcclo_tout("invalid"), "tout must be numeric or a list of numeric values")
})
