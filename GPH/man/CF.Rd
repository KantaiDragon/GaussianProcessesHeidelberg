\name{Covariance Functions}
\alias{CF}
\title{Covariance Functions}
\usage{
linear_covariance(x, y)
constant_covariance(x1, y1, x2, y2, nugget)
polynomial_covariance(x1, y1, x2, y2, range, sill, exponent)
squared_exponential_covariance(x1, y1, x2, y2, range, sill)
}
\description{
Following functions calc the covariance between 2 point:

	- linear: In this function, x1, y1, x2, and y2 are the coordinates of two points for which we want to calculate the covariance. range is the
 	spatial range parameter, and sill is the covariance value at the range distance.
	The function calculates the Euclidean distance between the two points, and then returns the linear covariance value using the equation: sill
 	* (1 - d / range), where d is the distance between the two points. If the distance is greater than the range, the covariance value is 0.
	Note that this is a very basic example of a linear covariance function, and there are many more complex covariance functions available
	 in external packages like gstat and spatial.

	- constant: In this function, x1, y1, x2, and y2 are the coordinates of two points for which we want to calculate the covariance, and nugget
	 is the covariance value for identical points.
	The function checks if the two points are identical (i.e., their coordinates are the same), and returns the constant covariance value if
	they are, or 0 if they are not.
	Note that this is a very basic example of a covariance function, and it assumes no spatial dependence between data points. More complex
	covariance functions are typically used in spatial data analysis to model spatial dependence between data points.

	- polynomial: In this function, x1, y1, x2, and y2 are the coordinates of two points for which we want to calculate the covariance. range
	is the spatial range parameter, sill is the covariance value at the range distance, and exponent is the degree of the polynomial (i.e., the order
	of the polynomial plus one).
	The function calculates the Euclidean distance between the two points, and then returns the polynomial covariance value using the equation:
 	sill * (1 - (d / range)^exponent), where d is the distance between the two points. If the distance is greater than the range, the covariance value
 	is 0.
	Note that this is a basic example of a polynomial covariance function, and there are many more complex covariance functions available in
 	external packages like gstat and spatial.

	- squared exponential: In this function, x1, y1, x2, and y2 are the coordinates of two points for which we want to calculate the covariance.
 	range is the spatial range parameter, and sill is the covariance value at the range distance.
	The function calculates the Euclidean distance between the two points, and then returns the squared exponential covariance value using the
 	equation: sill * exp(-(d^2) / (2 * range^2)), where d is the distance between the two points.
	Note that this is a basic example of a squared exponential covariance function, and there are many more complex covariance functions
 	available in external packages like gstat and spatial.
}
\examples{
# Example usage linear covariance
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 6, 8, 10)
# Calculate the covariance between x and y
lin_cov(x, y)
}
