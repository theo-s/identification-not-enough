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

experiment_1 <- run_sim(p_score = ps,
                        mu_1 = linear,
                        n=10000)
print(experiment_1)

experiment_2 <- run_sim(p_score = ps,
                        mu_1 = smooth,
                        n=10000)
print(experiment_2)

experiment_3 <- run_sim(p_score = ps,
                        mu_1 = nonlinear,
                        n=10000)
print(experiment_3)



