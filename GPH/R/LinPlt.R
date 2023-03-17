
plot_posterior_linear_covariance <- function(X, y, alpha, beta, sigma_squared) {
  library(ggplot2)
  # Calculate the posterior linear covariance
  X_tilde <- cbind(1, X)
  V <- solve(t(X_tilde) %*% X_tilde / sigma_squared + diag(rep(1/alpha, ncol(X_tilde))))
  cov_beta <- V %*% t(X_tilde) %*% y / sigma_squared
  # Plot the posterior linear covariance
  ggplot(data = data.frame(beta = beta, cov_beta = cov_beta), aes(x = beta, y = cov_beta)) +
    geom_line() +
    xlab(expression(beta)) +
    ylab(expression(paste("Covariance of ", hat(beta)))) +
    ggtitle("Posterior Linear Covariance") +
    theme_bw()
}


