#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 3L) {
  stop("Incorrect number of arguments (3).")
}

n <- as.integer(args[1L])
DGP <- as.integer(args[2L])
nrounds <- as.integer(args[3L])

# See bottom of file for run logic

run_sim <- function(n, DGP, nrounds) {
  results <- t(replicate(nrounds, run_sim_round(n, DGP)))
  list(
    n = n, DGP = DGP, nrounds = nrounds,
    bias = colMeans(results) - 0.5,
    rmse = sqrt(colMeans((results - 0.5)^2))
  )
}

run_sim_round <- function(n, DGP) {
  df <- data.frame(fh = (1:n <= n / 2), w = runif(n))
  df$ps <- if (DGP == 1L) {
    with(df, 1 / (1 + exp(2.2 - 4.4 * w)))
  } else if (DGP == 2L) {
    with(df, (9 - 2 * sin(6)^2 + 18 * w + 12 * sin(12 * w)) / 36)
  } else if (DGP == 3L) {
    with(df, 0.9 - 0.8 * (ceiling(100 * n * w) %% 2))
  } else {
    stop("Unknown DGP.")
  }
  df$x <- as.logical(rbinom(n, 1, df$ps))
  df$y <- ifelse(df$x, rbinom(n, 1, df$ps), NA)
  
  df$ps_glm <- winsorize(glm_predict(x ~ w, df))
  df$ps_loess <- winsorize(loess_predict(x ~ w, df))
  
  df$y_glm <- glm_predict(y ~ w, subset(df, x), df)
  df$y_loess <- loess_predict(y ~ w, subset(df, x), df)
  
  df$y_cglm <- NA
  df$y_cglm[df$fh] <- glm_predict(y ~ w, subset(df, !fh & x), subset(df, fh))
  df$y_cglm[!df$fh] <- glm_predict(y ~ w, subset(df, fh & x), subset(df, !fh))
  
  df$y_cloess <- NA
  df$y_cloess[df$fh] <- loess_predict(y ~ w, subset(df, !fh & x), subset(df, fh))
  df$y_cloess[!df$fh] <- loess_predict(y ~ w, subset(df, fh & x), subset(df, !fh))
  
  with(df, c(
    glm = est_y(y, x, y_glm),
    loess = est_y(y, x, y_loess),
    dr_glm = est_dr(y, x, ps_glm, y_glm),
    dr_loess = est_dr(y, x, ps_loess, y_loess),
    ht = est_ht(y, x, ps),
    aht_glm = est_dr(y, x, ps, y_cglm),
    aht_loess = est_dr(y, x, ps, y_cloess)
  ))
}

# Helper functions

glm_predict <- function(formula, data, newdata = data) {
  glm_model <- glm(formula, family = binomial(link = "logit"), data = data)
  predict(glm_model, newdata = newdata, type = "response")
}

loess_predict <- function(formula, data, newdata = data) {
  est_n <- nrow(data)
  loess_model <- loess(formula, data = data, span = 5 / est_n^(1/3), degree = 1)
  out <- predict(loess_model, newdata = newdata)
  ifelse(is.na(out), 0.5, out)
}

winsorize <- function(ps, cutoff = 0.01) {
  ifelse(ps < cutoff, cutoff, ps)
}

est_y <- function(y, x, ypred) {
  mean(ifelse(x, y, ypred))
}

est_ht <- function(y, x, ps) {
  sum(y[x] / ps[x]) / length(x)
}

est_dr <- function(y, x, ps, ypred) {
  mean(ypred) + est_ht(y - ypred, x, ps)
}


# Run logic
set.seed(12345)

#n <- 1e3; DGP <- 2L; nrounds <- 500
results <- run_sim(n, DGP, nrounds)

filename <- paste0("res/", paste("ine", n, DGP, nrounds, as.integer(Sys.time()), sep = "-"), ".rds")
saveRDS(results, file = filename)
