# generate afixed set of coefficients so we estimate the same ATE each time
set.seed(99)
# Uncomment to generate new coefs
# coefs <- 2*rbinom(1e6, 1, .5)-1
# saveRDS(object = coefs, file = "code/coefs.RDS")

# Logistic Potential Outcomes
logistic <- function(x, p, seed=1) {
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
  current_coefs <- c(current_coefs[1:round(p*ncol(x))], rep(0,ncol(x)-round(p*ncol(x))))

  # Get p scores with logistic transform
  outcomes <-  as.matrix(x) %*% current_coefs

  quants <- unname(quantile(outcomes, probs = c(.2,.8)))
  # Truncate Outcomes at +-1
  outcomes <- ifelse(outcomes < quants[1], quants[1],
                     ifelse(outcomes > quants[2], quants[2],
                            outcomes))

  return(outcomes)
}


# Calculate the ATE for experiment 1 -------------------------------------------
p <- c(.9)
n <- c(1e2,1e3,1e4)
k <- c(1000)
ate1 <- expand.grid(p,n,k)
ate1$ATE <- NA
colnames(ate1) <- c("P","N","K","ATE")

# Calculate ATE's for experiment 1= linear outcomes
for (p_i in p) {
  for (n_i in n) {
    for (k_i in k) {
      # Calculate and store the appropriate ate
      print(paste0(p_i," ",n_i," ",k_i))
      # Calculate ATE with monte carlo
      pulls <- c()
      for (seed_i in 1:1000) {
        set.seed(seed_i)

        X <- matrix(runif(n_i*k_i), ncol = k_i)
        X <- data.frame(X)

        outcome <- linear(x = X, p = p_i, seed = seed_i)
        pulls <- c(pulls,mean(outcome))
      }
      ate <- mean(pulls)

      ate1$ATE[which(ate1$P == p_i & ate1$N == n_i & ate1$K == k_i)] <- ate
    }
  }
}

saveRDS(object = ate1, file = "code/ate1.RDS")

# ATE for experiment 2 ---------------------------------------------------------
ate2 <- expand.grid(p,n,k)
ate2$ATE <- NA
colnames(ate2) <- c("P","N","K","ATE")
# Calculate ATE's for experiment 2= logistic outcomes
for (p_i in p) {
  for (n_i in n) {
    for (k_i in k) {
      # Calculate and store the appropriate ate
      print(paste0(p_i," ",n_i," ",k_i))
      # Calculate ATE with monte carlo
      pulls <- c()
      for (seed_i in 1:1000) {
        set.seed(seed_i)

        X <- matrix(runif(n_i*k_i), ncol = k_i)
        X <- data.frame(X)

        outcome <- logistic(x = X, p = p_i, seed = seed_i)
        pulls <- c(pulls,mean(outcome))
      }
      ate <- mean(pulls)

      ate2$ATE[which(ate2$P == p_i & ate2$N == n_i & ate2$K == k_i)] <- ate
    }
  }
}

saveRDS(object = ate2, file = "code/ate2.RDS")

# ATE for experiment 3 ---------------------------------------------------------
ate3 <- expand.grid(p,n,k)
ate3$ATE <- NA
colnames(ate3) <- c("P","N","K","ATE")
# Calculate ATE's for experiment 2= logistic outcomes
for (p_i in p) {
  for (n_i in n) {
    for (k_i in k) {
      print(paste0(p_i," ",n_i," ",k_i))
      # Calculate ATE with monte carlo
      pulls <- c()
      for (seed_i in 1:1000) {
        set.seed(seed_i)

        X <- matrix(runif(n_i*k_i), ncol = k_i)
        X <- data.frame(X)

        outcome <- truncated_linear(x = X, p = p_i, seed = seed_i)
        pulls <- c(pulls,mean(outcome))
      }
      ate <- mean(pulls)

      ate3$ATE[which(ate3$P == p_i & ate3$N == n_i & ate3$K == k_i)] <- ate
    }
  }
}

saveRDS(object = ate3, file = "code/ate3.RDS")
