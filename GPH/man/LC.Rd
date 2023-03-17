\name{Plot posterior linear covariance}
\alias{Plot LC}
\title{Plot posterior linear covariance}
\usage{
plot_posterior_linear_covariance(X, y, alpha, beta, sigma_squared)
}
\description{
The resulting plot will show the posterior density of the linear covariance function based on the given parameters.
}
\examples{
# Generate some data
set.seed(123)
n <- 50
X <- runif(n)
y <- 2*X + rnorm(n)

# Set hyperparameters
alpha <- 0.01
sigma_squared <- 1

# Evaluate the posterior linear covariance at these beta values
beta <- seq(-5, 5, length.out = 100)

# Plot the posterior linear covariance
plot_posterior_linear_covariance(X, y, alpha, beta, sigma_squared)

}
