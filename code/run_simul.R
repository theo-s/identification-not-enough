# Define Several potential outcome functions -----------------------------------
source("simul_utils.R")

# When running on the cluster, allow to specify n and seed through arguments ---
if (!interactive()){

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
ps <- function(x) {
  return(min(max(x,.01), .99))
}

# Linear potential outcome, true ATE = .25
linear <- function(x) {
  probs <- .5*x
  return(rbinom(p = probs,
                n = length(probs),
                size = 1))
}

# Smooth sinusoida potential outcome, true ATE ~= .2058
smooth <- function(x) {
  probs <- .05*sin(15*x)+.4*x
  return(rbinom(p = probs,
                n = length(probs),
                size = 1))
}

# Highly nonlinear potential outcome, with many local jumps and # bins >> n. True ATE = .25
nonlinear <- function(x) {
  # For now we keep the number of bins to be 100 X N depending on whatever N is
  n_bins <- 100*length(x)
  bin_width <- 1/n_bins
  probs <- ifelse(x %% bin_width < (bin_width/2),
                  .9*x,
                  .1*x)

  return(rbinom(p = probs,
                n = length(probs),
                size = 1))
}

# Run a sim of each DGP -------------------------------------------------------

res <- data.frame(N = NA,
                  Exp = NA,
                  nn1 = NA,
                  nn3 = NA,
                  ht = NA,
                  adjusted_ht = NA,
                  ps1 = NA,
                  ps3 = NA,
                  lr = NA,
                  rf = NA)

for (n in c(100, 200, 400, 800, 1600, 3200, 6400)) {
  experiment_1 <- run_sim(p_score = ps,
                          mu_1 = linear,
                          n=n,
                          seed=seed)

  res <- rbind(res, c(n,
                      1,
                      experiment_1$nn1,
                      experiment_1$nn3,
                      experiment_1$ht,
                      experiment_1$adjusted_ht,
                      experiment_1$ps1,
                      experiment_1$ps3,
                      experiment_1$lr,
                      experiment_1$rf))
  print(experiment_1)

  experiment_2 <- run_sim(p_score = ps,
                          mu_1 = smooth,
                          n=n,
                          seed=seed)
  print(experiment_2)
  res <- rbind(res, c(n,
                      2,
                      experiment_2$nn1,
                      experiment_2$nn3,
                      experiment_2$ht,
                      experiment_2$adjusted_ht,
                      experiment_2$ps1,
                      experiment_2$ps3,
                      experiment_2$lr,
                      experiment_2$rf))

  experiment_3 <- run_sim(p_score = ps,
                          mu_1 = nonlinear,
                          n=n,
                          seed=seed)
  print(experiment_3)
  res <- rbind(res, c(n,
                      3,
                      experiment_3$nn1,
                      experiment_3$nn3,
                      experiment_3$ht,
                      experiment_3$adjusted_ht,
                      experiment_3$ps1,
                      experiment_3$ps3,
                      experiment_3$lr,
                      experiment_3$rf))
}

filename <- paste0("results/sim_res_",seed,".RDS")
saveRDS(res[-1,], filename)



