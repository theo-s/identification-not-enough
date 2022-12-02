# Utility functions for use in run_simul.R -------------------------------------

# TMLE Methods -----------------------------------------------------------------
tmle_sl <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores,
  M = 1
) {
  library(tmle)

  if (length(unique(Y_train[Tr_train == 1])) == 1) {
    return(mean(Y_train[Tr_train == 1]))
  }

  # Add check for 1 dimensional case
  if (!is.data.frame(X_train)) {
    X_train = data.frame(V1 = X_train)
  }

  es <- tmle(Y = Y_train,
             W = X_train,
             A = Tr_train,
             g.SL.library = c("SL.glm", "SL.gam","SL.glmnet","SL.xgboost"),
             g.Delta.SL.library = c("SL.glm", "SL.gam","SL.glmnet","SL.xgboost"),
             Q.SL.library = c("SL.glm", "SL.gam","SL.glmnet","SL.xgboost")
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


  if (length(unique(Y_train[Tr_train == 1])) == 1) {
    return(mean(Y_train[Tr_train == 1]))
  }

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

  # Get estimate sequence with superlearner
  gn_seq <- build_gn_seq(A = Tr_train,
                         W = X_train,
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

# NO P Score -------------------------------------------------------------------
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

lasso <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {

  X_train <- data.frame(X_train)

  if (length(unique(Y_train[Tr_train == 1])) == 1) {
    return(mean(Y_train[Tr_train == 1]))
  }
  model <- cv.glmnet(x = as.matrix(X_train[Tr_train == 1,]),
                     y = Y_train[Tr_train == 1])

  pred_cate <- predict(model, newx = as.matrix(X_train[Tr_train == 1,]), s = "lambda.min")

  return(mean(pred_cate))
}

# Estimated P Score ------------------------------------------------------------
dr_lasso <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(glmnet)
  X_train <- data.frame(X_train)

  data <- X_train
  y <- Y_train
  tr <- Tr_train

  if (length(unique(y)) == 1) {
    y_pred <- rep(mean(y),nrow(data))
  } else {
    fit_1 <- cv.glmnet(x = as.matrix(data),
                       y = y)
    y_pred <- predict(fit_1, newx = as.matrix(data), s = "lambda.min")
  }



  fit <- glm(Tr_train ~.,
             data = data.frame(X_train,
                               Tr_train = Tr_train),
             family = "binomial")

  ps_estimate <- predict(fit,
                         newdata = data.frame(X_train),
                         type = "response")
  ps_estimate <- ifelse(ps_estimate > .99, .99,
                        ifelse(ps_estimate < .01, .01,
                               ps_estimate))

  ate <- mean((y-y_pred)*tr/ps_estimate + y_pred)
  return(ate)
}


dr_lasso_cf <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(glmnet)
  sample_1 <- sample(1:nrow(X_train), size = round(.5*nrow(X_train)), replace = FALSE)
  sample_2 <- (1:nrow(X_train))[!(1:nrow(X_train) %in% sample_1)]

  X_train <- data.frame(X_train)

  data_1 <- X_train[sample_1,]
  data_2 <- X_train[sample_2,]

  y_1 <- Y_train[sample_1]
  y_2 <- Y_train[sample_2]

  tr_1 <- Tr_train[sample_1]
  tr_2 <- Tr_train[sample_2]


  if (length(unique(y_1)) == 1 || length(unique(y_2)) == 1) {
    y_pred_1 <- rep(mean(y_1),nrow(data_1))
    y_pred_2 <- rep(mean(y_2),nrow(data_2))
  } else {
    fit_1 <- cv.glmnet(x = as.matrix(data_1),
                       y = y_1)
    fit_2 <- cv.glmnet(x = as.matrix(data_2),
                       y = y_2)

    y_pred_1 <- predict(fit_2, newx = as.matrix(data_1), s = "lambda.min")
    y_pred_2 <- predict(fit_1, newx = as.matrix(data_2), s = "lambda.min")
  }

  # Estimate P Scores
  pfit_1 <- glm(Tr_train ~.,
                data = data.frame(data_1,
                                  Tr_train = tr_1),
                family = "binomial")

  ps_estimate_1 <- predict(pfit_1,
                         newdata = data.frame(data_2),
                         type = "response")
  ps_estimate_1 <- ifelse(ps_estimate_1 > .99, .99,
                        ifelse(ps_estimate_1 < .01, .01,
                               ps_estimate_1))

  # Estimate P Scores 2
  pfit_2 <- glm(Tr_train ~.,
                data = data.frame(data_2,
                                  Tr_train = tr_2),
                family = "binomial")

  ps_estimate_2 <- predict(pfit_2,
                           newdata = data.frame(data_1),
                           type = "response")
  ps_estimate_2 <- ifelse(ps_estimate_2 > .99, .99,
                          ifelse(ps_estimate_2 < .01, .01,
                                 ps_estimate_2))


  ate_1 <- mean((y_1-y_pred_1)*tr_1/ps_estimate_1 + y_pred_1)
  ate_2 <- mean((y_2-y_pred_2)*tr_2/ps_estimate_2 + y_pred_2)
  return(.5*ate_1 + .5*ate_2)
}

# True P Score -----------------------------------------------------------------
ht_lasso <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(glmnet)

  X_train <- data.frame(X_train)

  data <- X_train
  y <- Y_train
  tr <- Tr_train
  p <- p_scores

  if (length(unique(y)) == 1) {
    y_pred <- rep(mean(y),nrow(data))
  } else {
    fit_1 <- cv.glmnet(x = as.matrix(data),
                       y = y)
    y_pred <- predict(fit_1, newx = as.matrix(data), s = "lambda.min")
  }


  ate <- mean((y-y_pred)*tr/p + y_pred)
  return(ate)
}


ht_lasso_cf <- function(
  X_train,
  Tr_train,
  Y_train,
  p_scores
) {
  library(glmnet)
  X_train <- data.frame(X_train)

  sample_1 <- sample(1:nrow(X_train), size = round(.5*nrow(X_train)), replace = FALSE)
  sample_2 <- (1:nrow(X_train))[!(1:nrow(X_train) %in% sample_1)]

  data_1 <- X_train[sample_1,]
  data_2 <- X_train[sample_2,]

  y_1 <- Y_train[sample_1]
  y_2 <- Y_train[sample_2]

  tr_1 <- Tr_train[sample_1]
  tr_2 <- Tr_train[sample_2]

  p_1 <- p_scores[sample_1]
  p_2 <- p_scores[sample_2]



  if (length(unique(y_1)) == 1 || length(unique(y_2)) == 1) {
    y_pred_1 <- rep(mean(y_1),nrow(data_1))
    y_pred_2 <- rep(mean(y_2),nrow(data_2))
  } else {
    fit_1 <- cv.glmnet(x = as.matrix(data_1),
                       y = y_1)
    fit_2 <- cv.glmnet(x = as.matrix(data_2),
                       y = y_2)

    y_pred_1 <- predict(fit_2, newx = as.matrix(data_1), s = "lambda.min")
    y_pred_2 <- predict(fit_1, newx = as.matrix(data_2), s = "lambda.min")
  }

  ate_1 <- mean((y_1-y_pred_1)*tr_1/p_1 + y_pred_1)
  ate_2 <- mean((y_2-y_pred_2)*tr_2/p_2 + y_pred_2)
  return(.5*ate_1 + .5*ate_2)
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

# Running the Sims -------------------------------------------------------------

# run_sim() runs one run of simulations for a given propensity score function
# and potential outcome function.
#
# Parameters:
#   n : Number of observations to simulate
#   n_test : Number of test observations to evaluate the estimators with.
#   p_score : Function to use to generate the propensity score
#   mu_1 : Function to use to generate the potential outcomes under treatment
#   p : the percent of covariates which have signal
#
# Return:
#   results : List of results containing the MSE for each estimator
run_sim <- function(
  p_score,
  mu_1,
  n = 500,
  k = 10,
  p = .1,
  seed = 100
) {

  # Set seed for cluster sims
  set.seed(seed)

  X <- matrix(runif(n*k), ncol = k)
  X <- data.frame(X)
  p_scores <- p_score(x = X, p = p, seed = seed)
  Tr <- sapply(p_scores, function(x){return(rbinom(1,1,prob=x))})
  outcome <- mu_1(x = X, p = p, seed = seed)
  Y <- ifelse(Tr,outcome,0)

  print("X: ")
  print(X[1:20,])
  print("P Scores: ")
  print(p_scores[1:20])
  print("Treatment: ")
  print(Tr[1:20])
  print("Outcome: ")
  print(Y[1:20])

  results <- list()

  results[["tmle"]] <- try(tmle_sl(X_train = X,
                            Y_train = Y,
                            Tr_train = Tr,
                            p_scores = p_scores))

  results[["tmle_c"]] <- try(tmle_c(X_train = X,
                                   Y_train = Y,
                                   Tr_train = Tr,
                                   p_scores = p_scores))

  results[["nn_matching"]] <- try(nn_matching(X_train = X,
                                     Y_train = Y,
                                     Tr_train = Tr,
                                     p_scores = p_scores))

  results[["lasso"]] <- try(lasso(X_train = X,
                               Y_train = Y,
                               Tr_train = Tr,
                               p_scores = p_scores))


  results[["dr_lasso"]] <- try(dr_lasso(X_train = X,
                                  Y_train = Y,
                                  Tr_train = Tr,
                                  p_scores = p_scores))

  results[["dr_lasso_cf"]] <- try(dr_lasso_cf(X_train = X,
                                     Y_train = Y,
                                     Tr_train = Tr,
                                     p_scores = p_scores))

  results[["ht_lasso"]] <- try(ht_lasso(X_train = X,
                                  Y_train = Y,
                                  Tr_train = Tr,
                                  p_scores = p_scores))

  results[["ht_lasso_cf"]] <- try(ht_lasso_cf(X_train = X,
                                     Y_train = Y,
                                     Tr_train = Tr,
                                     p_scores = p_scores))

  results[["ht"]] <- try(ht(X_train = X,
                            Y_train = Y,
                            Tr_train = Tr,
                            p_scores = p_scores))

  results[["ps_matching_true"]] <- try(ps_matching_true(X_train = X,
                                                        Y_train = Y,
                                                        Tr_train = Tr,
                                                        p_scores = p_scores))

  return(results)

}
