\name{Weight Space View}
\alias{WSV}
\title{Weight Space View}
\usage{
weight_space_view(X, y)
}
\description{
In this implementation, X is the matrix of training examples and y is the vector of corresponding labels. The weight_space_view function calculates the pseudoinverse of X using the solve function and then uses this to calculate the weight vector w. Finally, the function returns the weight vector.
In the example usage, we create a matrix X and a vector y of training examples and labels, respectively. We then call the weight_space_view function
with these arguments to get the weight vector w. We print out w to the console to see the result.
}
\examples{
# Example usage
# Create a matrix of training examples
X <- matrix(c(1, 2, 1, 1, 0, 2, 1, 1, 1), ncol = 3)
# Create a vector of training labels
y <- c(3, 2, 1)
# Call the weight space view function to get the weight vector
w <- weight_space_view(X, y)
# Print the weight vector
print(w)
}
