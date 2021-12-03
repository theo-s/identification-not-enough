# Define Several potential outcome functions -----------------------------------
source("code/simul_utils_high.R")

# When running on the cluster, allow to specify n and seed through arguments ---
if (!interactive()){
  library(argparse)
  parse_arguments <- ArgumentParser()
  # We can pass an array of different seeds if we want to run different reps
  # on the cluster
  parse_arguments$add_argument("--s",
                               type = "double",
                               default = 100,
                               help = "seed for rep")

  arguments <- parse_arguments$parse_args()

  seed <- arguments$s
} else {
  seed <- 100
}

# P score function that is logistic with the probabilities truncated at .1, and .9
# based on a logistic regression model with the coefficients coming from IID
# rademacher random variables on P percent of the features
ps <- function(x, p, seed=1) {
  # Pad coefficients with zeros
  coefs <- readRDS("code/coefs.RDS")
  current_coefs <- coefs[1:ncol(x)]
  if (p == 0) {
    current_coefs <- rep(0, ncol(x))
  } else {
    current_coefs <- c(current_coefs[1:round(p*ncol(x))], rep(0,ncol(x)-round(p*ncol(x))))
  }

  # Get p scores with logistic transform
  probs <-  as.matrix(x) %*% current_coefs
  logit <- function(x){return(exp(x)/(1+exp(x)))}
  probs <- sapply(probs, logit)

  # Truncate P scores at .1,.9
  probs <- ifelse(probs < .1,.1,ifelse(probs > .9,.9, probs))

  return(probs)
}

# Logistic Potential Outcomes
logistic <- function(x, p, seed=1) {
  # Pad coefficients with zeros
  coefs <- readRDS("code/coefs.RDS")
  current_coefs <- coefs[1:ncol(x)]

  if (p == 0) {
    current_coefs <- rep(0, ncol(x))
  } else {
    current_coefs <- c(current_coefs[1:round(p*ncol(x))], rep(0,ncol(x)-round(p*ncol(x))))
    current_coefs <- current_coefs *(1/sqrt(nrow(x)))
  }

  # Get p scores with logistic transform
  probs <-  as.matrix(x) %*% current_coefs
  logit <- function(x){return(exp(x)/(1+exp(x)))}
  probs <- sapply(probs, logit)

  # Truncate P scores at .1,.9
  probs <- ifelse(probs < .1,.1,ifelse(probs > .9,.9, probs))

  return(rbinom(p = probs,
                n = length(probs),
                size = 1))
}

# Linear Potential Outcomes
linear <- function(x, p, seed=1) {
  # Pad coefficients with zeros
  coefs <- readRDS("code/coefs.RDS")
  current_coefs <- coefs[1:ncol(x)]
  if (p == 0) {
    current_coefs <- rep(0, ncol(x))
  } else {
    current_coefs <- c(current_coefs[1:round(p*ncol(x))], rep(0,ncol(x)-round(p*ncol(x))))
    current_coefs <- current_coefs *(1/sqrt(nrow(x)))
  }

  # Get p scores with logistic transform
  outcomes <-  as.matrix(x) %*% current_coefs

  return(outcomes)
}

# Truncated Linear Potential Outcomes
truncated_linear <- function(x, p, seed=1) {
  # Pad coefficients with zeros
  coefs <- readRDS("code/coefs.RDS")
  current_coefs <- coefs[1:ncol(x)]
  if (p == 0) {
    current_coefs <- rep(0, ncol(x))
  } else {
    current_coefs <- c(current_coefs[1:round(p*ncol(x))], rep(0,ncol(x)-round(p*ncol(x))))
    current_coefs <- current_coefs *(1/sqrt(nrow(x)))
  }

  # Get p scores with logistic transform
  outcomes <-  as.matrix(x) %*% current_coefs

  quants <- unname(quantile(outcomes, probs = c(.2,.8)))
  # Truncate Outcomes at +-1
  outcomes <- ifelse(outcomes < quants[1],quants[1],ifelse(outcomes > quants[2],quants[2], outcomes))

  return(outcomes)
}

# Run a sim of each DGP -------------------------------------------------------
res <- data.frame(N = NA,
                  K = NA,
                  P = NA,
                  Exp = NA,
                  tmle = NA,
                  nn_matching = NA,
                  lasso = NA,
                  dr_lasso = NA,
                  dr_lasso_cf = NA,
                  ht_lasso = NA,
                  ht_lasso_cf = NA,
                  ht = NA,
                  ps_matching_true = NA)

for (n in c(100,1000,10000)) {
    for (k in c(round(.1*n), n, 10*n)) {
      for (p in c(.1,.5,.9)) {

        experiment_1 <- run_sim(p_score = ps,
                                mu_1 = linear,
                                n=n,
                                k=k,
                                p=p,
                                seed=seed)


        res <- rbind(res, c(n,
                            k,
                            p,
                            1,
                            experiment_1$tmle,
                            experiment_1$nn_matching,
                            experiment_1$lasso,
                            experiment_1$dr_lasso,
                            experiment_1$dr_lasso_cf,
                            experiment_1$ht_lasso,
                            experiment_1$ht_lasso_cf,
                            experiment_1$ht,
                            experiment_1$ps_matching_true))
        print(res)

        experiment_2 <- run_sim(p_score = ps,
                                mu_1 = logistic,
                                n=n,
                                k=k,
                                p=p,
                                seed=seed)

        res <- rbind(res, c(n,
                            k,
                            p,
                            2,
                            experiment_2$tmle,
                            experiment_2$nn_matching,
                            experiment_2$lasso,
                            experiment_2$dr_lasso,
                            experiment_2$dr_lasso_cf,
                            experiment_2$ht_lasso,
                            experiment_2$ht_lasso_cf,
                            experiment_2$ht,
                            experiment_2$ps_matching_true))
        print(res)

        experiment_3 <- run_sim(p_score = ps,
                                mu_1 = truncated_linear,
                                n=n,
                                k=k,
                                p=p,
                                seed=seed)

        res <- rbind(res, c(n,
                            k,
                            p,
                            3,
                            experiment_3$tmle,
                            experiment_3$nn_matching,
                            experiment_3$lasso,
                            experiment_3$dr_lasso,
                            experiment_3$dr_lasso_cf,
                            experiment_3$ht_lasso,
                            experiment_3$ht_lasso_cf,
                            experiment_3$ht,
                            experiment_3$ps_matching_true))
        print(res)
      }
    }
  }


filename <- paste0("code/results_high/res_",seed,".RDS")
saveRDS(res[-1,], filename)



