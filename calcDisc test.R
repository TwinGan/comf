test_that("test calcDisc function", {
  # Test 1: Simple input
  result1 <- calcDisc(ta=25, tr=25, vel=0.1, rh=50, clo=0.5, met=1)
  expect_true(result1 > 0, info = "Expected a positive discomfort index")
  
  # Test 2: Edge case with higher temperature
  result2 <- calcDisc(ta=35, tr=35, vel=0.1, rh=50, clo=0.5, met=1)
  expect_true(result2 > result1, info = "Expected higher discomfort with higher temperature")
  
  # Test 3: Variation in humidity
  result3_low_humidity <- calcDisc(ta=25, tr=25, vel=0.1, rh=10, clo=0.5, met=1)
  result3_high_humidity <- calcDisc(ta=25, tr=25, vel=0.1, rh=90, clo=0.5, met=1)
  expect_true(result3_high_humidity > result3_low_humidity, info = "Expected higher discomfort with higher humidity")
  
  # Test 4: Error handling for extreme conditions
  expect_error(calcDisc(ta=60, tr=60, vel=0.1, rh=50, clo=0.5, met=1), "Expected an error with extreme temperatures")
})
