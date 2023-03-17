# Define the weight space view function
weight_space_view <- function(X, y) {

  # Calculate the pseudoinverse of X
  X_pinv <- solve(t(X) %*% X) %*% t(X)

  # Calculate the weight vector
  w <- X_pinv %*% y

  return(w)
}

