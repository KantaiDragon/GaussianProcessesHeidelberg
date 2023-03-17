bayesian_model_selection <- function(data, model_list) {
  # Calculate the log-likelihood for each model
  log_likelihoods <- sapply(model_list, function(model) {
    sum(dnorm(data, mean = model[1], sd = model[2], log = TRUE))
  })
  # Calculate the number of parameters in each model
  num_params <- sapply(model_list, function(model) {
    length(model)
  })
  # Calculate the BIC for each model
  n <- length(data)
  bic <- -2 * log_likelihoods + num_params * log(n)
  # Identify the model with the lowest BIC
  best_model <- model_list[which.min(bic)]
  # Return the best model and its BIC
  return(list(best_model = best_model, bic = min(bic)))
}

