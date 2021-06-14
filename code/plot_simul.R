library(ggplot2)
library(dplyr)
library(viridis)
library(reshape)

# Plot Bias --------------------------------------------------------------------
bias_table <- read.csv(file = "code/BIAStable.csv")

# Plot Experiment 1 ------------------------------------------------------------
abs(bias_table) %>%
  filter(Exp == 1) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "Nearest Neighbor Matching (M=1)",
                                                       "nn3_1" = "Nearest Neighbor Matching (M=3)",
                                                       "ps1_1" = "Propensity Score Matching (M=1)",
                                                       "ps3_1" = "Propensity Score Matching (M=3)",
                                                       "lr_1" = "Logit Regression",
                                                       "rf_1" = "Random Forest",
                                                       "adjusted_ht_1" = "Adjusted Horvitz-Thompson",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_color_viridis_d()+
  theme_bw()+
  labs(y = "|Bias| when estimating ATE", x = "Sample Size", title = "Experiment 1")

ggsave("code/figures/bias_experiment1.pdf", height = 6, width = 6)

# Plot Experiment 2 ------------------------------------------------------------
abs(bias_table) %>%
  filter(Exp == 2) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "Nearest Neighbor Matching (M=1)",
                                                       "nn3_1" = "Nearest Neighbor Matching (M=3)",
                                                       "ps1_1" = "Propensity Score Matching (M=1)",
                                                       "ps3_1" = "Propensity Score Matching (M=3)",
                                                       "lr_1" = "Logit Regression",
                                                       "rf_1" = "Random Forest",
                                                       "adjusted_ht_1" = "Adjusted Horvitz-Thompson",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_color_viridis_d()+
  theme_bw()+
  labs(y = "|Bias| when estimating ATE", x = "Sample Size", title = "Experiment 2")

ggsave("code/figures/bias_experiment2.pdf", height = 6, width = 6)

# Plot Experiment 3 ------------------------------------------------------------
abs(bias_table) %>%
  filter(Exp == 3) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "Nearest Neighbor Matching (M=1)",
                                                       "nn3_1" = "Nearest Neighbor Matching (M=3)",
                                                       "ps1_1" = "Propensity Score Matching (M=1)",
                                                       "ps3_1" = "Propensity Score Matching (M=3)",
                                                       "lr_1" = "Logit Regression",
                                                       "rf_1" = "Random Forest",
                                                       "adjusted_ht_1" = "Adjusted Horvitz-Thompson",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line()+
  scale_color_viridis_d()+
  theme_bw()+
  labs(y = "|Bias| when estimating ATE", x = "Sample Size", title = "Experiment 3")

ggsave("code/figures/bias_experiment3.pdf", height = 6, width = 9)

# Plot sqrt(var) --------------------------------------------------------------------
mse_table <- read.csv(file = "code/MSEtable.csv")
rmse_table <- mse_table
rmse_table[,-c(1:3)] <- sqrt(mse_table[,-c(1:3)])

# Plot Experiment 1 ------------------------------------------------------------
rmse_table %>%
  filter(Exp == 1) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "Nearest Neighbor Matching (M=1)",
                                                       "nn3_1" = "Nearest Neighbor Matching (M=3)",
                                                       "ps1_1" = "Propensity Score Matching (M=1)",
                                                       "ps3_1" = "Propensity Score Matching (M=3)",
                                                       "lr_1" = "Logit Regression",
                                                       "rf_1" = "Random Forest",
                                                       "adjusted_ht_1" = "Adjusted Horvitz-Thompson",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_color_viridis_d()+
  theme_bw()+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 1")

ggsave("code/figures/rmse_experiment1.pdf", height = 6, width = 6)

# Plot Experiment 2 ------------------------------------------------------------
rmse_table %>%
  filter(Exp == 2) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "Nearest Neighbor Matching (M=1)",
                                                       "nn3_1" = "Nearest Neighbor Matching (M=3)",
                                                       "ps1_1" = "Propensity Score Matching (M=1)",
                                                       "ps3_1" = "Propensity Score Matching (M=3)",
                                                       "lr_1" = "Logit Regression",
                                                       "rf_1" = "Random Forest",
                                                       "adjusted_ht_1" = "Adjusted Horvitz-Thompson",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_color_viridis_d()+
  theme_bw()+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 2")

ggsave("code/figures/rmse_experiment2.pdf", height = 6, width = 6)


# Plot Experiment 3 ------------------------------------------------------------
rmse_table %>%
  filter(Exp == 3) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "Nearest Neighbor Matching (M=1)",
                                                       "nn3_1" = "Nearest Neighbor Matching (M=3)",
                                                       "ps1_1" = "Propensity Score Matching (M=1)",
                                                       "ps3_1" = "Propensity Score Matching (M=3)",
                                                       "lr_1" = "Logit Regression",
                                                       "rf_1" = "Random Forest",
                                                       "adjusted_ht_1" = "Adjusted Horvitz-Thompson",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line()+
  scale_color_viridis_d()+
  theme_bw()+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 3")

ggsave("code/figures/rmse_experiment3.pdf", height = 6, width = 9)
