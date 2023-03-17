\name{Plot posterior squared exponential covariance}
\alias{Plot SEC}
\title{Plot posterior squared exponential covariance}
\usage{
plot_posterior_sqexp_covariance(sigma_sq, alpha, beta, x, l = 1)
}
\description{
sigma_sq: The prior estimate of the variance.

   		 alpha: The prior precision parameter.

    		 beta: The prior scaling parameter.

    		 x: The vector of observations.

   		 l: The length scale parameter.

		 The function computes the posterior mean and variance of a squared exponential covariance function, and then plots the posterior density function of the
		 covariance. The plot shows the density of the posterior distribution, with the mean indicated by a vertical line.

		 The resulting plot will show the posterior density of the squared exponential covariance function based
		on the given parameters. The mean of the posterior distribution will be indicated by a vertical line on the plot.
}
\examples{
# Define prior parameters
sigma_sq <- 1.5
alpha <- 1
beta <- 1
l <- 0.5
# Define data parameters
x <- c(1.2, 0.9, 1.1, 0.8, 1.3, 1.5, 1.4, 1.6, 1.2, 1.1)

# Call the function to plot the posterior
plot_posterior_sqexp_covariance(sigma_sq, alpha, beta, x, l)

}
