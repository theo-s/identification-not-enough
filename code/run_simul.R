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

res <- data.frame(N = NA,
                  Exp = NA,
                  tmle= NA,
                  tmle_c= NA,
                  tmle_hal= NA,
                  dr_logit= NA,
                  cross_fit= NA,
                  dr_RF_Pscore_CF= NA,
                  dr_RF_Pscore= NA,
                  dr_noCF= NA,
                  ht= NA,
                  adjusted_ht= NA,
                  loop_rf= NA,
                  nn1= NA,
                  ps1= NA,
                  ps_rf1= NA,
                  ps_logit1= NA,
                  rf= NA,
                  lr= NA
                  )

# res <- data.frame(N = NA,
#                   Exp = NA,
#                   dr_logit = NA,
#                   dr_RF_Pscore_CF = NA,
#                   dr_RF_Pscore = NA,
#                   dr_noCF = NA,
#                   cross_fit = NA)

res <- data.frame(N = NA,
                  Exp = NA,
                  #tmle = NA,
                  tmle_c = NA)

for (n in c(100, 1e6)) {
  experiment_1 <- run_sim(p_score = linear_ps,
                          mu_1 = linear,
                          n=n,
                          seed=seed)

  res <- rbind(res, c(n,
                      1,
                      experiment_1$tmle,
                      experiment_1$tmle_c,
                      experiment_1$tmle_hal,
                      experiment_1$dr_logit,
                      experiment_1$cross_fit,
                      experiment_1$dr_RF_Pscore_CF,
                      experiment_1$dr_RF_Pscore,
                      experiment_1$dr_noCF,
                      experiment_1$ht,
                      experiment_1$adjusted_ht,
                      experiment_1$loop_rf,
                      experiment_1$nn1,
                      experiment_1$ps1,
                      experiment_1$ps_rf1,
                      experiment_1$ps_logit1,
                      experiment_1$rf,
                      experiment_1$lr
                      ))
  print(res)

  experiment_2 <- run_sim(p_score = smooth_ps,
                          mu_1 = smooth,
                          n=n,
                          seed=seed)

  res <- rbind(res, c(n,
                      2,
                      experiment_2$tmle,
                      experiment_2$tmle_c,
                      experiment_2$tmle_hal,
                      experiment_2$dr_logit,
                      experiment_2$cross_fit,
                      experiment_2$dr_RF_Pscore_CF,
                      experiment_2$dr_RF_Pscore,
                      experiment_2$dr_noCF,
                      experiment_2$ht,
                      experiment_2$adjusted_ht,
                      experiment_2$loop_rf,
                      experiment_2$nn1,
                      experiment_2$ps1,
                      experiment_2$ps_rf1,
                      experiment_2$ps_logit1,
                      experiment_2$rf,
                      experiment_2$lr
                      ))
  print(res)

  experiment_3 <- run_sim(p_score = nonlinear_ps,
                          mu_1 = nonlinear,
                          n=n,
                          seed=seed)

  res <- rbind(res, c(n,
                      3,
                      experiment_3$tmle,
                      experiment_3$tmle_c,
                      experiment_3$tmle_hal,
                      experiment_3$dr_logit,
                      experiment_3$cross_fit,
                      experiment_3$dr_RF_Pscore_CF,
                      experiment_3$dr_RF_Pscore,
                      experiment_3$dr_noCF,
                      experiment_3$ht,
                      experiment_3$adjusted_ht,
                      experiment_3$loop_rf,
                      experiment_3$nn1,
                      experiment_3$ps1,
                      experiment_3$ps_rf1,
                      experiment_3$ps_logit1,
                      experiment_3$rf,
                      experiment_3$lr
                      ))
  print(res)
}

filename <- paste0("code/results_ultra/new_sim_res_",seed,".RDS")
saveRDS(res[-1,], filename)



