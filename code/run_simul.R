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
  probs <-  ifelse(probs > .99, .99,
                   ifelse( probs < .01, .01, probs))

  logit <- function(x){return(exp(x)/(1+exp(x)))}
  return(sapply(probs, logit))
}

# Linear potential outcome, true ATE = .5861
linear <- function(x) {
  probs <-  .5*x + .1
  probs <-  ifelse(probs > .99, .99,
                   ifelse( probs < .01, .01, probs))

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
                  nn_matching = NA,
                  logistic = NA,
                  rf = NA,
                  dr_logit = NA,
                  dr_rf = NA,
                  dr_rf_cf = NA,
                  ht = NA,
                  loop_rf = NA,
                  ps_matching_true = NA,
                  ht_rf_cf = NA,
                  ht_rf = NA)

for (n in c(100,1e4,1e5,1e6)) {
  for (experiment_i in 1:3) {
    print(paste0("Running experiment ", experiment_i))

    current.args = list()
    current.args[["n"]] <- n
    current.args[["seed"]] <- seed

    # Get the right DGP
    if (experiment_i == 1) {
      current.args[["p_score"]] <- linear_ps
      current.args[["mu_1"]] <- linear
    } else if (experiment_i == 2) {
      current.args[["p_score"]] <- smooth_ps
      current.args[["mu_1"]] <- smooth
    } else if (experiment_i == 3) {
      current.args[["p_score"]] <- nonlinear_ps
      current.args[["mu_1"]] <- nonlinear
    } else {
      stop(paste0("Not supported experiment id",experiment_i))
    }

    experiment <- do.call(what = run_sim,
                          args = current.args)

    res <- rbind(res, c(n,1, sapply(1:length(experiment),
                                    function(idx){return(experiment[[names(res)[2+idx]]])})))

    print(res)
  }
}

filename <- paste0("code/results_2-6-23/res_",seed,".RDS")
saveRDS(res[-1,], filename)



