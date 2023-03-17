plot_posterior_const_covariance <- function(sigma_sq, a, b, n, x) {
  # Compute posterior parameters
  sigma_sq_post <- 1 / (1/sigma_sq + n/b)
  mu_post <- a / (b * n) + x / sigma_sq
  # Generate plot
  x_vals <- seq(mu_post - 3*sqrt(sigma_sq_post), mu_post + 3*sqrt(sigma_sq_post), length.out = 100)
  y_vals <- dnorm(x_vals, mean = mu_post, sd = sqrt(sigma_sq_post))
  plot(x_vals, y_vals, type = "l", xlab = "Covariance", ylab = "Density", main = "Posterior Constant Covariance")
}

