library(testthat)

test_heat_index <- function() {
  expect_equal(calcHeatIndex(25, 50), 25.9)
  expect_equal(calcHeatIndex(30, 80), 37.7)
  expect_equal(calcHeatIndex(77, 50, units = "IP"), 78.6)
  expect_equal(calcHeatIndex(86, 80, units = "IP"), 99.8)
}

# Run the test
test_heat_index()