test_that("test_calcpmvpdd", {
  # Source required files
  source("../config.R")
  source("../utils-test-tool.R")
  source("calcPMVPPD.R")
  
  # Call retrieve_data() to get the test data (adapted for the PMV/PPD data)
  # Update the URL to the appropriate one
  reference_tables <- retrieve_data(url_config$test_pmv_ppd_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data
  
  tolerance_pmv <- tolerance$pmv
  tolerance_ppd <- tolerance$ppd
  
  # Loop through each dataset and test the results
  for (i in seq_along(data)) {
    inputs <- data[[i]]$inputs
    expected <- data[[i]]$outputs
    
    # Run the calcPMVPPD function with the current test case inputs
    result <- calcPMVPPD(inputs$ta,
                         inputs$tr,
                         inputs$rh,
                         inputs$v,
                         inputs$met,
                         inputs$clo)
    
# Compare the results with the expected values using expect_true and absolute difference
    expect_true(abs(result$pmv - expected$pmv) < tolerance_pmv, 
                info = paste("Test case", i, "failed on PMV values"))
    expect_true(abs(result$ppd - expected$ppd) < tolerance_ppd, 
                info = paste("Test case", i, "failed on PPD values"))
  }
})

