# Utility functions for use in run_simul.R -------------------------------------

# Estimators -------------------------------------------------------------------

# TMLE Methods -----------------------------------------------------------------
tmle_sl <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores,
  M = 1
) {
  library(tmle)
  library(hal9001)

  # Add check for 1 dimensional case
  if (!is.data.frame(X_train)) {
    X_train = data.frame(V1 = X_train)
  }

  es <- tmle(Y = Y_train,
             W = X_train,
             A = Tr_train,
             g.SL.library = c("SL.glm", "SL.gam","SL.hal9001"),
             g.Delta.SL.library = c("SL.glm", "SL.gam","SL.hal9001"),
             Q.SL.library = c("SL.glm", "SL.gam","SL.hal9001")
  )

  return(es$estimates$ATE$psi)
}

tmle_hal <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores,
  M = 1
) {
  library(tmle)
  library(hal9001)

  # Add check for 1 dimensional case
  if (!is.data.frame(X_train)) {
    X_train = data.frame(V1 = X_train)
  }

  es <- tmle(Y = Y_train,
             W = X_train,
             A = Tr_train,
             g.SL.library = "SL.hal9001",
             g.Delta.SL.library = "SL.hal9001",
             Q.SL.library = "SL.hal9001")

  return(es$estimates$ATE$psi)
}


tmle_c <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores,
  M = 1
) {
  library(ctmle)


  if (length(unique(Y_train[Tr_train == 1])) == 1) {
    return(mean(Y_train[Tr_train == 1]))
  }

  # Add check for 1 dimensional case
  if (!is.data.frame(X_train)) {
    X_train = data.frame(V1 = X_train)
  }

  SL.library = c("SL.glm", "SL.gam","SL.glmnet","SL.xgboost")

  N <- nrow(X_train)
  V <- 5

  # Initialize Q
  Q <- cbind(rep(mean(Y_train[Tr_train == 0]), N),
             rep(mean(Y_train[Tr_train == 1]), N))

  # Make folds
  folds <-by(sample(1:N,N), rep(1:V, length=N), list)

  X_train$ones <- rep(1, nrow(X_train))

  # Get estimate sequence with superlearner
  gn_seq <- build_gn_seq(A = Tr_train,
                         W = as.matrix(X_train),
                         SL.library = SL.library,
                         folds = folds)


  es <- ctmleGeneral(Y = Y_train,
                     W = X_train,
                     A = Tr_train,
                     Q = Q,
                     ctmletype = 1,
                     gn_candidates = gn_seq$gn_candidates,
                     gn_candidates_cv = gn_seq$gn_candidates_cv,
                     folds = folds,
                     V = 5
  )

  return(es$est)
}


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

ps_matching_true <- function(
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

ps_matching_rf <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores,
  M = 1
) {
  library(Matching)
  library(ranger)

  X_train = data.frame(v1 = X_train)

  # Estimate the Pscore using RF
  fit <- ranger(x = X_train,
               y = Tr_train)

  ps_estimate <- predict(fit, X_train)$predictions
  ps_estimate <- ifelse(ps_estimate > .99, .99,
                        ifelse(ps_estimate < .01, .01,
                               ps_estimate))

  estimate <- Match(Y = Y_train,
                    Tr = Tr_train,
                    X = ps_estimate,
                    estimand = "ATE",
                    ties = FALSE,
                    M = M)
  return(estimate$est[1,1])
}

ps_matching_logit <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores,
  M = 1
) {
  library(Matching)

  fit <- glm(Tr_train ~.,
             data = data.frame(X_train = X_train,
                               Tr_train = Tr_train),
             family = "binomial")

  ps_estimate <- predict(fit,
                         newdata = data.frame(X_train = X_train),
                         type = "response")
  ps_estimate <- ifelse(ps_estimate > .99, .99,
                        ifelse(ps_estimate < .01, .01,
                               ps_estimate))

  estimate <- Match(Y = Y_train,
                    Tr = Tr_train,
                    X = ps_estimate,
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

cross_fit <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(ranger)
  sample_1 <- sample(1:length(X_train), size = round(.5*length(X_train)), replace = FALSE)
  sample_2 <- (1:length(X_train))[!(1:length(X_train) %in% sample_1)]

  data_1 <- data.frame(v1 = X_train[sample_1])
  data_2 <- data.frame(v1 = X_train[sample_2])

  y_1 <- Y_train[sample_1]
  y_2 <- Y_train[sample_2]

  tr_1 <- Tr_train[sample_1]
  tr_2 <- Tr_train[sample_2]

  p_1 <- p_scores[sample_1]
  p_2 <- p_scores[sample_2]

  fit_1 <- ranger(x = data_1,
                  y = y_1)

  fit_2 <- ranger(x = data_2,
                  y = y_2)

  y_pred_1 <- predict(fit_2, data_1)$predictions
  y_pred_2 <- predict(fit_1, data_2)$predictions

  ate_1 <- mean((y_1-y_pred_1)*tr_1/p_1 + y_pred_1)
  ate_2 <- mean((y_2-y_pred_2)*tr_2/p_2 + y_pred_2)
  return(.5*ate_1 + .5*ate_2)
}

dr_noCF <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(ranger)

  data <- data.frame(v1 = X_train)
  y <- Y_train
  tr <- Tr_train
  p <- p_scores


  fit_1 <- ranger(x = data,
                  y = y)

  y_pred <- predict(fit_1, data)$predictions

  ate <- mean((y-y_pred)*tr/p + y_pred)
  return(ate)
}

dr_RF_Pscore <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(ranger)

  data <- data.frame(v1 = X_train)
  y <- Y_train
  tr <- Tr_train
  p <- p_scores


  fit_1 <- ranger(x = data,
                  y = y)

  fit_2 <- ranger(x = data,
                  y = Tr_train)

  y_pred <- predict(fit_1, data)$predictions
  p_pred <- predict(fit_2, data)$predictions

  # Truncate the P scores
  p_trunc <- ifelse(p_pred < .01, .01, ifelse(p_pred > .99, .99, p_pred))

  ate <- mean((y-y_pred)*tr/p_trunc + y_pred)
  return(ate)
}

# Wrapper function, given samples p, e, and y, uses a cross fitting estimator to estimate the
# ATE doing covariate adjustment based on samp_y, propensity score estimation
# based on samp_p, and the estimate on samp_e
dr_wrapper <- function(
  X_train,
  Y_train,
  Tr_train,
  sample_1,
  sample_2,
  sample_3
) {
  data_1 <- data.frame(v1 = X_train[sample_1])
  data_2 <- data.frame(v1 = X_train[sample_2])
  data_3 <- data.frame(v1 = X_train[sample_3])


  y_1 <- Y_train[sample_1]
  y_2 <- Y_train[sample_2]
  y_3 <- Y_train[sample_3]

  tr_1 <- Tr_train[sample_1]
  tr_2 <- Tr_train[sample_2]
  tr_3 <- Tr_train[sample_3]

  # Run regression estimates
  fit <- ranger(x = data_1,
                y = y_1)
  p_fit <- ranger(x = data_2,
                  y = tr_2)

  y_pred <- predict(fit, data_3)$predictions
  p_pred <- predict(p_fit, data_3)$predictions

  p_trunc <- ifelse(p_pred < .01, .01, ifelse(p_pred > .99, .99, p_pred))

  ate <- mean((y_3-y_pred)*tr_3/p_trunc + y_pred)
  return(ate)
}

dr_RF_Pscore_CF <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(ranger)
  library(caret)

  folds <- caret::createFolds(1:length(X_train), k=3)
  sample_1 <- folds$Fold1
  sample_2 <- folds$Fold2
  sample_3 <- folds$Fold3


  ate_1 <- dr_wrapper(X_train, Y_train, Tr_train,sample_1, sample_2, sample_3)
  ate_2 <- dr_wrapper(X_train, Y_train, Tr_train,sample_3, sample_1, sample_2)
  ate_3 <- dr_wrapper(X_train, Y_train, Tr_train,sample_2, sample_3, sample_1)
  return(ate_1)
}



dr_logit <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  model_outcome <- glm(Y_train ~.,
                       data = data.frame(X_train = X_train,
                                         Y_train = Y_train),
                       family = "binomial")

  pred_out <- predict(model_outcome,
                      newdata = data.frame(X_train = X_train),
                      type = "response")

  model_p <- glm(Tr_train ~.,
                 data = data.frame(X_train = X_train,
                                   Tr_train = Tr_train),
                 family = "binomial")

  pred_p <- predict(model_p,
                    newdata = data.frame(Tr_train = Tr_train),
                    type = "response")

  cate <- Tr_train*Y_train / pred_p - (Tr_train - pred_p)*pred_out/pred_p
  return(mean(cate))
}

loop_rf <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(Rforestry)

  data_c <- data.frame(v1 = X_train[Tr_train == 0])
  data_t <- data.frame(v1 = X_train[Tr_train == 1])

  forest_c <- Rforestry::forestry(x = data_c,
                                  y = Y_train[Tr_train == 0],
                                  ntree = 500)

  forest_t <- Rforestry::forestry(x = data_t,
                                  y = Y_train[Tr_train == 1],
                                  ntree = 500)

  # Use OOB predictions on the correct sets
  y0_preds_c <- predict(forest_c, data_c, aggregation = "oob")
  y0_preds_t <- predict(forest_c, data_t)

  y1_preds_c <- predict(forest_t, data_c)
  y1_preds_t <- predict(forest_t, data_t, aggregation = "oob")

  y0_pred <- rep(0, length(Y_train))
  y0_pred[Tr_train == 1] <- y0_preds_t
  y0_pred[Tr_train == 0] <- y0_preds_c

  y1_pred <- rep(0, length(Y_train))
  y1_pred[Tr_train == 1] <- y1_preds_t
  y1_pred[Tr_train == 0] <- y1_preds_c

  m_hat <- (1-p_scores)*y1_pred + p_scores*y0_pred
  U <- ifelse(Tr_train, 1/p_scores , - 1/(1-p_scores))
  tau_hat <- (Y_train - m_hat)* U
  return(mean(tau_hat))
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
  if (is.vector(X)) {
    p_scores <- p_score(X)
  } else {
    p_scores <- sapply(X, p_score)
  }

  Tr <- mu_1(X)
  Y <- ifelse(Tr, mu_1(X),0)
  sd <- sqrt(1/(snr*var(Y)))
  Y <- Y #+ rnorm(n, sd = sd) # Noiseless for now

  results <- list()


#  results[["tmle"]] <- try(tmle_sl(X_train = X,
#                                    Y_train = Y,
#                                    Tr_train = Tr,
#                                    p_scores = p_scores))

  results[["tmle_c"]] <- try(tmle_c(X_train = X,
                                    Y_train = Y,
                                    Tr_train = Tr,
                                    p_scores = p_scores))

#  results[["tmle_hal"]] <- try(tmle_hal(X_train = X,
#                                        Y_train = Y,
#                                        Tr_train = Tr,
#                                        p_scores = p_scores))


#  results[["dr_logit"]] <- try(dr_logit(X_train = X,
#                                        Y_train = Y,
#                                        Tr_train = Tr,
#                                        p_scores = p_scores))

#  results[["cross_fit"]] <- try(cross_fit(X_train = X,
#                                          Y_train = Y,
#                                          Tr_train = Tr,
#                                          p_scores = p_scores))

#  results[["dr_RF_Pscore_CF"]] <- try(dr_RF_Pscore_CF(X_train = X,
#                                                      Y_train = Y,
#                                                      Tr_train = Tr,
#                                                      p_scores = p_scores))

#  results[["dr_RF_Pscore"]] <- try(dr_RF_Pscore(X_train = X,
#                                        Y_train = Y,
#                                        Tr_train = Tr,
#                                        p_scores = p_scores))

#  results[["dr_noCF"]] <- try(dr_noCF(X_train = X,
#                                        Y_train = Y,
#                                        Tr_train = Tr,
#                                        p_scores = p_scores))


  results[["ht"]] <- try(ht(X_train = X,
                            Y_train = Y,
                            Tr_train = Tr,
                            p_scores = p_scores))

#  results[["adjusted_ht"]] <- try(adjusted_ht(X_train = X,
#                                              Y_train = Y,
#                                              Tr_train = Tr,
#                                              p_scores = p_scores))

#  results[["loop_rf"]] <- try(loop_rf(X_train = X,
#                                      Y_train = Y,
#                                      Tr_train = Tr,
#                                      p_scores = p_scores))

#  results[["nn1"]] <- try(nn_matching(X_train = X,
#                                      Y_train = Y,
#                                      Tr_train = Tr,
#                                      p_scores = p_scores,
#                                      M=1))

#  results[["ps1"]] <- try(ps_matching_true(X_train = X,
#                                           Y_train = Y,
#                                           Tr_train = Tr,
#                                           p_scores = p_scores,
#                                           M=1))

#  results[["ps_rf1"]] <- try(ps_matching_rf(X_train = X,
#                                            Y_train = Y,
#                                            Tr_train = Tr,
#                                            p_scores = p_scores,
#                                            M=1))

#  results[["ps_logit1"]] <- try(ps_matching_logit(X_train = X,
#                                                  Y_train = Y,
#                                                  Tr_train = Tr,
#                                                  p_scores = p_scores,
#                                                  M=1))

#  results[["rf"]] <- try(rf(X_train = X,
#                            Y_train = Y,
#                            Tr_train = Tr,
#                            p_scores = p_scores))

  results[["lr"]] <- try(lr(X_train = X,
                            Y_train = Y,
                            Tr_train = Tr,
                            p_scores = p_scores))

  return(results)

}
