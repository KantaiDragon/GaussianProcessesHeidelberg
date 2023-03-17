plot_posterior_sqexp_covariance <- function(sigma_sq, alpha, beta, x, l = 1) {
  # Compute posterior parameters
  n <- length(x)
  K <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      K[i, j] <- beta * exp(-0.5 * (x[i] - x[j])^2 / l^2)
    }
  }
  L <- chol(alpha * K + sigma_sq * diag(n))
  alpha_post <- solve(t(L) %*% L, x / sigma_sq)
  sigma_sq_post <- (1 / beta - t(alpha_post) %*% K %*% alpha_post + t(alpha_post) %*% solve(L, solve(t(L), alpha_post))) / n
  # Generate plot
  x_vals <- seq(min(x), max(x), length.out = 100)
  y_vals <- dnorm(x_vals, mean = 0, sd = sqrt(sigma_sq_post))
  for (i in 1:length(x)) {
    y_vals <- y_vals * dnorm(x_vals, mean = alpha_post[i], sd = sqrt(sigma_sq_post * exp(-0.5 * (x_vals - x[i])^2 / l^2)))
  }
  plot(x_vals, y_vals, type = "l", xlab = "Covariance", ylab = "Density", main = "Posterior Squared Exponential Covariance")
}
