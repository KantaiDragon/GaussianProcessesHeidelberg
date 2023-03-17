# Function to calculate the linear covariance
linear_covariance <- function(x, y) {
  n <- length(x) # Length of vectors
  mean_x <- mean(x) # Mean of x vector
  mean_y <- mean(y) # Mean of y vector
  # Calculate the covariance
  sum <- 0
  for (i in 1:n) {
    sum <- sum + ((x[i] - mean_x) * (y[i] - mean_y))
  }
  # Return the covariance
  sum / (n - 1)
}

constant_covariance <- function(x1, y1, x2, y2, nugget) {
  if (x1 == x2 & y1 == y2) {
    return(nugget)  # Constant covariance value for identical points
  } else {
    return(0)  # Covariance is 0 for non-identical points
  }
}

polynomial_covariance <- function(x1, y1, x2, y2, range, sill, exponent) {
  d <- sqrt((x1 - x2)^2 + (y1 - y2)^2)  # Euclidean distance between the two points
  if (d <= range) {
    return(sill * (1 - (d / range)^exponent))  # Polynomial covariance function
  } else {
    return(0)  # Covariance is 0 if distance is greater than the range
  }
}

# Define a function that calculates the squared exponential covariance between two points
squared_exponential_covariance <- function(x1, y1, x2, y2, range, sill) {
  d <- sqrt((x1 - x2)^2 + (y1 - y2)^2)  # Euclidean distance between the two points
  return(sill * exp(-(d^2) / (2 * range^2)))  # Squared exponential covariance function
}
