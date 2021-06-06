# Define Several potential outcome functions -----------------------------------

# truncated p score
ps <- function(x) {
  return(min(max(x,.01), .99))
}

# Linear potential outcome, true ATE = .25
linear <- function(x) {
  return(.5*x)
}

# Smooth sinusoida potential outcome, true ATE ~= .2058
smooth <- function(x) {
  return(.05*sin(15*x)+.4*x)
}

# Highly nonlinear potential outcome, with many local jumps and # bins >> n. True ATE = .25
nonlinear <- function(x) {
  return(ifelse(x %% .01 < .005,
                .9*x,
                .1*x))
}

# Plot the potential outcomes --------------------------------------------------
x <- runif(1000)
plot(x, sapply(x,linear))
plot(x, sapply(x,smooth))
plot(x, sapply(x,nonlinear))


# Run a sim of each DGP --------------------------------------------------------
source("simul_utils.R")

res <- data.frame(N = NA, Exp = NA, nn1 = NA, ht = NA)

for (n in c(100, 200, 400, 800, 1600, 3200, 6400)) {
  experiment_1 <- run_sim(p_score = ps,
                          mu_1 = linear,
                          n=n)

  res <- rbind(res, c(n, 1, experiment_1$nn1, experiment_1$ht))
  print(experiment_1)

  experiment_2 <- run_sim(p_score = ps,
                          mu_1 = smooth,
                          n=n)
  print(experiment_2)
  res <- rbind(res, c(n, 2, experiment_2$nn1,experiment_2$ht))

  experiment_3 <- run_sim(p_score = ps,
                          mu_1 = nonlinear,
                          n=n)
  print(experiment_3)
  res <- rbind(res, c(n, 3, experiment_3$nn1,experiment_3$ht))
}

saveRDS(res[-1,], "res.RDS")



