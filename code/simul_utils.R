# Utility functions for use in run_simul.R -------------------------------------


# run_sim() runs one run of simulations for a given propensity score function
# and potential outcome function.
#
# Parameters:
#   n : Number of observations to simulate
#   p_score : Function to use to generate the propensity score
#   mu_1 : Function to use to generate the potential outcomes under treatment
#   true_p : Flag which specifies whether or not we give the estimators the true p score
#   snr : The signal to noise ratio to use for the simulation.
#
# Return:
#   results : List of results containing the MSE for each estimator
run_sim <- function(
  p_score,
  mu_1,
  n = 500,
  true_p = TRUE,
  snr = 3
) {

  X <- runif(n)
  p_scores <- sapply(X, p_score)
  flips <- runif(n)
  Tr <- ifelse(flips < p_scores,1,0)
  Y <- ifelse(Tr, sapply(X, mu_1),0)
  sd <- sqrt(1/(snr*var(Y)))
  Y <- Y + rnorm(n, sd = sd)

  results <- list()

}
