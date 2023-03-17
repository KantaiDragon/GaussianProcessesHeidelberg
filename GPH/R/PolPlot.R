plot_posterior_poly_covariance <- function(sigma_sq, alpha, beta, x, degree = 2) {
  # Compute posterior parameters
  n <- length(x)
  X <- matrix(c(rep(1, n), seq(0, 1, length.out = n)), ncol = degree + 1)
  Y <- x / sigma_sq
  V <- solve(alpha * t(X) %*% X + beta * diag(degree + 1))
  beta_post <- V %*% alpha * t(X) %*% Y
  sigma_sq_post <- (1 / beta + t(Y - X %*% beta_post) %*% alpha %*% (Y - X %*% beta_post)) / (n + alpha)
  # Generate plot
  x_vals <- seq(0, 1, length.out = 100)
  y_vals <- dnorm(x_vals, mean = beta_post[1], sd = sqrt(sigma_sq_post))
  for (i in 2:(degree+1)) {
    y_vals <- y_vals * dnorm(x_vals, mean = beta_post[i], sd = sqrt(sigma_sq_post))
  }
  plot(x_vals, y_vals, type = "l", xlab = "Covariance", ylab = "Density", main = "Posterior Polynomial Covariance")
}

