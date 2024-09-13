calc <- function(ta, rh) {
  # Convert input temperature and humidity to numeric vectors for calculation
  ta <- as.numeric(ta)
  rh <- as.numeric(rh)
  
  # Calculate the Discomfort Index (DI)
  di <- ta - 0.55 * (1 - 0.01 * rh) * (ta - 14.5)
  
  # Define DI categories
  di_categories <- c(
    "No discomfort",
    "Less than 50% feels discomfort",
    "More than 50% feels discomfort",
    "Most of the population feels discomfort",
    "Everyone feels severe stress",
    "State of medical emergency"
  )
  
  # Function to map DI value to a discomfort condition
  get_discomfort_condition <- function(di_value) {
    if (di_value < 21) {
      return(di_categories[1])
    } else if (di_value < 24) {
      return(di_categories[2])
    } else if (di_value < 27) {
      return(di_categories[3])
    } else if (di_value < 29) {
      return(di_categories[4])
    } else if (di_value < 32) {
      return(di_categories[5])
    } else {
      return(di_categories[6])
    }
  }
  
  # Apply discomfort condition mapping to all DI values
  discomfort_condition <- sapply(di, get_discomfort_condition)
  
  # Return the results as a list
  return(list(
    di = round(di, 1),
    discomfort_condition = discomfort_condition
  ))
}