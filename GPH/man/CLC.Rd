\name{Linear Covariance}
\alias{CLC}
\title{Linear Covariance}
\usage{
linear_covariance(x, y)
}
\description{
In this function, x and y are the coordinates of two points for which we want to calculate the covariance
	Note that this is a very basic example of a linear covariance function, and there are many more sophisticated covariance functions available
	 in external packages like gstat and spatial.
}
\examples{
# Example usage
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 6, 8, 10)
# Calculate the covariance between x and y
linear_covariance(x, y)
}
