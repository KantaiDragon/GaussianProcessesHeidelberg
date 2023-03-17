\name{Bayesian Modell Selection}
\alias{BMS}
\title{Bayesian Modell Selection}
\usage{
bayesian_model_selection(data, model_list)
}
\description{
The function will return a list containing the best model (i.e., the one with the lowest BIC) and its corresponding BIC value.
}
\examples{
my_data <- rnorm(100, mean = 5, sd = 2)
model1 <- c(4, 1.5)
model2 <- c(5.5, 2.5)
models <- list(model1, model2)
result <- bayesian_model_selection(my_data, models)
}
