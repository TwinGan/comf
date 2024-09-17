calcclo_tout <- function(tout, units = "SI") {
  # Check if tout is numeric or a list of numeric values
  if (!is.numeric(tout) && !is.list(tout)) {
    stop("tout must be numeric or a list of numeric values.")
  }
  
  # Convert list to numeric vector if necessary
  if (is.list(tout)) {
    tout <- unlist(tout)
    if (!is.numeric(tout)) {
      stop("Elements of tout list must be numeric values.")
    }
  }

  # Validate the units
  valid_units <- c("SI", "IP")
  if (!(toupper(units) %in% valid_units)) {
    stop(paste("Invalid unit:", units, ". Supported units are", toString(valid_units), "."))
  }

  # Convert units if necessary (from IP to SI)
  if (tolower(units) == "ip") {
    tout <- (tout - 32) * 5/9  # Convert Fahrenheit to Celsius
  }

  # Calculate clo based on tout
  clo <- ifelse(tout < 26, 10^(-0.1635 - 0.0066 * tout), 0.46)
  clo <- ifelse(tout < 5, 0.818 - 0.0364 * tout, clo)
  clo <- ifelse(tout < -5, 1, clo)

  # Round to 2 decimal places
  return(round(clo, 2))
}
