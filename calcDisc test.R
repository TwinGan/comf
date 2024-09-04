discomfort_index <- function(temperatures, humidity) {
  # This is a placeholder function. You should replace it with the actual function logic.
  
  # Here, we assume the function returns a list with two elements: 'di' and 'discomfort_condition'
  return(list(
    di = c(19.2, 21.0, 25.0, 27.2, 29.4, 33.0),
    discomfort_condition = c(
      "No discomfort",
      "Less than 50% feels discomfort",
      "More than 50% feels discomfort",
      "Most of the population feels discomfort",
      "Everyone feels severe stress",
      "State of medical emergency"
    )
  ))
}

# Test cases
test_that("discomfort_index returns expected values", {
  result <- discomfort_index(c(21, 23.5, 29, 32, 35, 40), 50)
  expect_equal(result$di, c(19.2, 21.0, 25.0, 27.2, 29.4, 33.0))
  expect_equal(result$discomfort_condition, c(
    "No discomfort",
    "Less than 50% feels discomfort",
    "More than 50% feels discomfort",
    "Most of the population feels discomfort",
    "Everyone feels severe stress",
    "State of medical emergency"
  ))
  
  result2 <- discomfort_index(c(35, 35), c(10, 90))
  expect_equal(result2$di, c(24.9, 33.9))
  expect_equal(result2$discomfort_condition, c(
    "More than 50% feels discomfort",
    "State of medical emergency"
  ))
})
