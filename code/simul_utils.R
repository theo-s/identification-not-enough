# Utility functions for use in run_simul.R -------------------------------------

# Estimators -------------------------------------------------------------------

ht <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(iWeigReg)

  estimate <- ate.HT(y = Y_train,
                     tr = Tr_train,
                     p = p_scores)
  return(estimate$mu[1])
}

nn_matching <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores,
  M = 1
) {
  library(Matching)
  estimate <- Match(Y = Y_train,
                    Tr = Tr_train,
                    X = X_train,
                    estimand = "ATE",
                    ties = FALSE,
                    M = M)
  return(estimate$est[1,1])

}

ps_matching <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores,
  M = 1
) {
  library(Matching)
  estimate <- Match(Y = Y_train,
                    Tr = Tr_train,
                    X = p_scores,
                    estimand = "ATE",
                    ties = FALSE,
                    M = M)
  return(estimate$est[1,1])

}

rf <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(causalToolbox)

  X_train <- data.frame(v1 = X_train)

  fit <- S_RF(
    feat = X_train,
    tr = Tr_train,
    yobs = Y_train,
    verbose = FALSE
  )

  # Estimating the cate here is very awkward, causalToolbox needs to be cleaned up 
  # to not throw an error when we only have one covariate
  estimated_cate <- predict(fit@forest, 
                            cbind(fit@forest@processed_dta$processed_x[,-2], 
                                  tr = 1), 
                            aggregation = "oob") - 
                    predict(fit@forest, 
                            cbind(fit@forest@processed_dta$processed_x[,-2], 
                                  tr = 0), 
                            aggregation = "oob")

  return(mean(estimated_cate))
}

lr <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  model <- glm(Y_train ~.,
               data = data.frame(X_train = X_train[Tr_train == 1],
                                 Y_train = Y_train[Tr_train == 1]),
               family = "binomial")

  pred_cate <- predict(model,
                       newdata = data.frame(X_train = X_train),
                       type = "response")

  return(mean(pred_cate))
}

adjusted_ht <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  model <- glm(Y_train ~.,
               data = data.frame(X_train = X_train[Tr_train == 1],
                                 Y_train = Y_train[Tr_train == 1],
                                 ps = 1/p_scores[Tr_train == 1]),
               family = "binomial")

  pred_out <- predict(model,
                      newdata = data.frame(X_train = X_train,
                                           ps = 1/p_scores),
                      type = "link")

  logit <- function(x){return(exp(x)/(1+exp(x)))}

  return(mean(sapply(pred_out, logit)))
}


# run_sim() runs one run of simulations for a given propensity score function
# and potential outcome function.
#
# Parameters:
#   n : Number of observations to simulate
#   n_test : Number of test observations to evaluate the estimators with.
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
  snr = 3,
  seed = 100
) {

  # Set seed for cluster sims
  set.seed(seed)

  X <- runif(n)
  p_scores <- sapply(X, p_score)
  flips <- runif(n)
  Tr <- ifelse(flips < p_scores,1,0)
  Y <- ifelse(Tr, sapply(X, mu_1),0)
  sd <- sqrt(1/(snr*var(Y)))
  Y <- Y #+ rnorm(n, sd = sd) # Noiseless for now

  results <- list()
  results[["ht"]] <- try(ht(X_train = X,
                            Y_train = Y,
                            Tr_train = Tr,
                            p_scores = p_scores))

  results[["adjusted_ht"]] <- try(adjusted_ht(X_train = X,
                                              Y_train = Y,
                                              Tr_train = Tr,
                                              p_scores = p_scores))

  results[["nn1"]] <- try(nn_matching(X_train = X,
                                      Y_train = Y,
                                      Tr_train = Tr,
                                      p_scores = p_scores,
                                      M=1))

  results[["nn3"]] <- try(nn_matching(X_train = X,
                                      Y_train = Y,
                                      Tr_train = Tr,
                                      p_scores = p_scores,
                                      M=3))

  results[["ps1"]] <- try(ps_matching(X_train = X,
                                      Y_train = Y,
                                      Tr_train = Tr,
                                      p_scores = p_scores,
                                      M=1))

  results[["ps3"]] <- try(ps_matching(X_train = X,
                                      Y_train = Y,
                                      Tr_train = Tr,
                                      p_scores = p_scores,
                                      M=3))

  results[["rf"]] <- try(rf(X_train = X,
                            Y_train = Y,
                            Tr_train = Tr,
                            p_scores = p_scores))

  results[["lr"]] <- try(lr(X_train = X,
                            Y_train = Y,
                            Tr_train = Tr,
                            p_scores = p_scores))

  return(results)

}
