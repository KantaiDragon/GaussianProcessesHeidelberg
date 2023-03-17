\name{Plot posterior polynomial covariance}
\alias{Plot PC}
\title{Plot posterior polynomial covariance}
\usage{
plot_posterior_poly_covariance(sigma_sq, alpha, beta, x, degree = 2)
}
\description{
This function takes five arguments:

   		 sigma_sq: The prior estimate of the covariance.

   		 alpha: The prior precision parameter.

   		 beta: The prior scaling parameter.

   		 x: The vector of observations.

   		 degree: The degree of the polynomial.

		The function computes the posterior mean and variance of the coefficients of a polynomial     covariance function, and then plots the posterior
		density function of the covariance. The plot shows the density of the posterior distribution, with the mean indicated by a vertical line.
}
\examples{
# Define prior parameters
sigma_sq <- 1.5
alpha <- 1
beta <- 1
degree <- 2
# Define data parameters
x <- c(1.2, 0.9, 1.1, 0.8, 1.3, 1.5, 1.4, 1.6, 1.2, 1.1)

# Call the function to plot the posterior
plot_posterior_poly_covariance(sigma_sq, alpha, beta, x, degree)

}
