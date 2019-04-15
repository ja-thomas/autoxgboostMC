# Get the iteration parameter of a fitted xboost model with early stopping
getBestIteration = function(mod) {
  getLearnerModel(mod, more.unwrap = TRUE)$best_iteration
}
