# Define Several potential outcome functions -----------------------------------
source("code/simul_utils.R")

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

# truncated p score
linear_ps <- function(x) {
  probs <-  .5*x + .1
  probs <-  min(max(probs,.01), .99)

  logit <- function(x){return(exp(x)/(1+exp(x)))}
  return(sapply(probs, logit))
}

# Linear potential outcome, true ATE = .5861
linear <- function(x) {
  probs <-  .5*x + .1
  probs <-  min(max(probs,.01), .99)

  logit <- function(x){return(exp(x)/(1+exp(x)))}
  probs <- sapply(probs, logit)
  return(rbinom(p = probs,
                n = length(probs),
                size = 1))
}

smooth_ps <- function(x) {
  probs <- .2*sin(15*x)+.4*x+.1
  return(probs)
}

# Smooth sinusoida potential outcome, true ATE ~= .32346
smooth <- function(x) {
  probs <- .2*sin(15*x)+.4*x+.1
  return(rbinom(p = probs,
                n = length(probs),
                size = 1))
}

nonlinear_ps <- function(x) {
  # For now we keep the number of bins to be 100 X N depending on whatever N is
  n_bins <- 100*length(x)
  bin_width <- 1/n_bins
  probs <- ifelse(x %% bin_width < (bin_width/2),
                  .9,
                  .1)

  return(probs)
}

# Highly nonlinear potential outcome, with many local jumps and # bins >> n. True ATE = .5
nonlinear <- function(x) {
  # For now we keep the number of bins to be 100 X N depending on whatever N is
  n_bins <- 100*length(x)
  bin_width <- 1/n_bins
  probs <- ifelse(x %% bin_width < (bin_width/2),
                  .9,
                  .1)

  return(rbinom(p = probs,
                n = length(probs),
                size = 1))
}

# Run a sim of each DGP -------------------------------------------------------

#res <- data.frame(N = NA,
#                  Exp = NA,
#                  nn1 = NA,
#                  ht = NA,
#                  adjusted_ht = NA,
#                  loop_rf = NA,
#                  ps1 = NA,
#                  ps_rf1 = NA,
#                  ps_logit1 = NA,
#                  lr = NA,
#                  rf = NA
#                  )

# res <- data.frame(N = NA,
#                   Exp = NA,
#                   dr_logit = NA,
#                   dr_RF_Pscore_CF = NA,
#                   dr_RF_Pscore = NA,
#                   dr_noCF = NA,
#                   cross_fit = NA)

res <- data.frame(N = NA,
                  Exp = NA,
                  tmle = NA,
                  tmle_hal = NA)

for (n in c(1000, 10000, 100000)) {
  experiment_1 <- run_sim(p_score = linear_ps,
                          mu_1 = linear,
                          n=n,
                          seed=seed)

  res <- rbind(res, c(n,
                      1,
                      experiment_1$tmle,
                      experiment_1$tmle_hal))
  print(res)

  experiment_2 <- run_sim(p_score = smooth_ps,
                          mu_1 = smooth,
                          n=n,
                          seed=seed)

  res <- rbind(res, c(n,
                      2,
                      experiment_2$tmle,
                      experiment_2$tmle_hal))
  print(res)

  experiment_3 <- run_sim(p_score = nonlinear_ps,
                          mu_1 = nonlinear,
                          n=n,
                          seed=seed)

  res <- rbind(res, c(n,
                      3,
                      experiment_3$tmle,
                      experiment_3$tmle_hal))
  print(res)
}

filename <- paste0("code/results_tmle/new_sim_res_",seed,".RDS")
saveRDS(res[-1,], filename)



