
test_that("test_calcdiscomfort_index", { 
  source("calcdiscomfort_index.r")
  source("../config.R")
  source("../utils-test-tool.R")
  
  # Call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_discomfort_index_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data
  
  # Loop through the rows of the test data
 for (i in seq_along(data)) {
  # Extract input and output data (assuming data is a list)
  inputs <- data[[i]]$inputs
  outputs <- data[[i]]$outputs

  # Vectorized calcdiscomfort_index to handle all inputs at once
  result <- calcdiscomfort_index(
    tdb = inputs$tdb,
    rh = inputs$rh
  )

  # Test the DI values with expected outputs, allowing for tolerance if necessary
  if (!all.equal(result$di, outputs$di, tolerance = tolerance$di)) {
    warning(paste("Test case", i, "failed on DI values"))
  }

  # Test the discomfort conditions with expected outputs
  if (!all.equal(result$discomfort_condition, outputs$discomfort_condition)) {
    warning(paste("Test case", i, "failed on discomfort conditions"))
  }
}
})