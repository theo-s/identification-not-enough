# generate afixed set of coefficients so we estimate the same ATE each time
set.seed(99)

coefs <- 2*rbinom(1e6, 1, .5)-1
saveRDS(object = coefs, file = "code/coefs.RDS")

# Calculate the ATE for a range of sparsity levels and dimensions
p <- c(0,.1,.5,.9)
n <- c(1e2,1e3,1e4,1e5)
k <- c(.1*n, n, 10*n)
ate1 <- expand.grid(p,n,k)
ate1$ATE <- NA
colnames(ate1) <- c("P","N","K","ATE")

# Calculate ATE's for experiment 1= linear outcomes
for (p_i in p) {
  for (n_i in n) {
    for (k_i in k) {

      # Calculate and store the appropriate ate
      num_coefs <- round(p_i*k_i)
      prop_coefs <- sum(coefs[1:num_coefs])/ n_i

      ate1$ATE[which(ate1$P == p_i & ate1$N == n_i & ate1$K == k_i)] <- prop_coefs*.5
    }
  }
}

saveRDS(object = ate1, file = "code/ate1.RDS")

ate2 <- expand.grid(p,n,k)
ate2$ATE <- NA
colnames(ate2) <- c("P","N","K","ATE")
# Calculate ATE's for experiment 2= logistic outcomes
for (p_i in p) {
  for (n_i in n) {
    for (k_i in k) {
      # Calculate and store the appropriate ate
      num_coefs <- round(p_i*k_i)
      prop_coefs <- sum(coefs[1:num_coefs])/ n_i

      ate2$ATE[which(ate2$P == p_i & ate2$N == n_i & ate2$K == k_i)] <- prop_coefs*.6201145069582775
    }
  }
}

saveRDS(object = ate2, file = "code/ate2.RDS")
