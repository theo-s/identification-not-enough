library(ggplot2)
library(dplyr)
library(viridis)
library(reshape)
library(scales)
library(ggrepel)

# Plot Bias --------------------------------------------------------------------
bias_table <- read.csv(file = "code/BIAStable.csv")

# Plot Experiment 1 ------------------------------------------------------------
abs(bias_table) %>%
  dplyr::select(-adjusted_ht_1) %>%
  dplyr::filter(Exp == 1) %>%
  dplyr::select(-X, -Exp) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 25600) -> end_values

abs(bias_table) %>%
  dplyr::select(-adjusted_ht_1) %>%
  filter(Exp == 1) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line(show.legend = FALSE)+
  xlim(0, 40000) +
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = c("#440154FF", "#443A83FF",
                                 "#31688EFF", "#21908CFF",
                                 "#35B779FF", "#8FD744FF",
                                 "#FDE725FF", "#D2E21BFF"),
    size = 3, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 25, nudge_y = 0, point.padding = .92, max.overlaps = Inf
  )+
  scale_color_viridis_d()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "|Bias| when estimating ATE", x = "Sample Size", title = "Experiment 1")

ggsave("code/figures/bias_experiment1.pdf", height = 4, width = 4)

# Plot Experiment 2 ------------------------------------------------------------
abs(bias_table) %>%
  dplyr::select(-adjusted_ht_1) %>%
  dplyr::filter(Exp == 2) %>%
  dplyr::select(-X, -Exp) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 25600) -> end_values

abs(bias_table) %>%
  filter(Exp == 2) %>%
  dplyr::select(-adjusted_ht_1) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line(show.legend = FALSE)+
  xlim(0, 40000) +
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = c("#440154FF", "#443A83FF",
                                 "#31688EFF", "#21908CFF",
                                 "#35B779FF", "#8FD744FF",
                                 "#FDE725FF", "#D2E21BFF"),
    size = 3, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 25, nudge_y = 0, point.padding = .92, max.overlaps = Inf
  )+
  scale_color_viridis_d()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "|Bias| when estimating ATE", x = "Sample Size", title = "Experiment 2")

ggsave("code/figures/bias_experiment2.pdf", height = 4, width = 4)

# Plot Experiment 3 ------------------------------------------------------------
abs(bias_table) %>%
  dplyr::select(-adjusted_ht_1) %>%
  dplyr::filter(Exp == 3) %>%
  dplyr::select(-X, -Exp) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 25600) -> end_values


abs(bias_table) %>%
  filter(Exp == 3) %>%
  dplyr::select(-adjusted_ht_1) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line(show.legend = FALSE)+
  xlim(0, 40000) +
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = c("#440154FF", "#443A83FF",
                                 "#31688EFF", "#21908CFF",
                                 "#35B779FF", "#8FD744FF",
                                 "#FDE725FF", "#D2E21BFF"),
    size = 3, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 25, nudge_y = 0, point.padding = .92, max.overlaps = Inf
  )+
  scale_color_viridis_d()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(colour = guide_legend(title.position = "top"))+
  theme(legend.position = "bottom",
        legend.key.height = unit(.6, 'cm'), #change legend key height
        legend.key.width = unit(.3, 'cm'),
        legend.key.size = unit(4.5, 'cm'))+
  labs(y = "|Bias| when estimating ATE", x = "Sample Size", title = "Experiment 3")

ggsave("code/figures/bias_experiment3.pdf", height = 4.67, width = 4)

# Plot sqrt(var) --------------------------------------------------------------------
mse_table <- read.csv(file = "code/MSEtable.csv")
rmse_table <- mse_table
rmse_table[,-c(1:3)] <- sqrt(mse_table[,-c(1:3)])

# Plot Experiment 1 ------------------------------------------------------------
rmse_table %>%
  dplyr::select(-adjusted_ht_1) %>%
  dplyr::filter(Exp == 1) %>%
  dplyr::select(-X, -Exp) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 25600) -> end_values

rmse_table %>%
  filter(Exp == 1) %>%
  dplyr::select(-adjusted_ht_1) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line(show.legend = FALSE)+
  xlim(0, 40000) +
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = c("#440154FF", "#443A83FF",
                                 "#31688EFF", "#21908CFF",
                                 "#35B779FF", "#8FD744FF",
                                 "#FDE725FF", "#D2E21BFF"),
    size = 3, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 25, nudge_y = 0, point.padding = .92, max.overlaps = Inf
  )+
  scale_color_viridis_d()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 1")

ggsave("code/figures/rmse_experiment1.pdf", height = 4, width = 4)

# Plot Experiment 2 ------------------------------------------------------------
rmse_table %>%
  dplyr::select(-adjusted_ht_1) %>%
  dplyr::filter(Exp == 2) %>%
  dplyr::select(-X, -Exp) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 25600) -> end_values

rmse_table %>%
  filter(Exp == 2) %>%
  dplyr::select(-adjusted_ht_1) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line(show.legend = FALSE)+
  xlim(0, 40000) +
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = c("#440154FF", "#443A83FF",
                                 "#31688EFF", "#21908CFF",
                                 "#35B779FF", "#8FD744FF",
                                 "#FDE725FF", "#D2E21BFF"),
    size = 3, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 25, nudge_y = 0, point.padding = .92, max.overlaps = Inf
  )+
  scale_color_viridis_d()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 2")

ggsave("code/figures/rmse_experiment2.pdf", height = 4, width = 4)


# Plot Experiment 3 ------------------------------------------------------------
rmse_table %>%
  dplyr::select(-adjusted_ht_1) %>%
  dplyr::filter(Exp == 3) %>%
  dplyr::select(-X, -Exp) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 25600) -> end_values

rmse_table %>%
  filter(Exp == 3) %>%
  dplyr::select(-adjusted_ht_1) %>%
  dplyr::select(-Exp, -X) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "P Score Matching (True)",
                                                       "ps_rf1_1" = "P Score Matching (RF)",
                                                       "ps_logit1_1" = "P Score Matching (Logistic)",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line(show.legend = FALSE)+
  xlim(0, 40000) +
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = c("#440154FF", "#443A83FF",
                                 "#31688EFF", "#21908CFF",
                                 "#35B779FF", "#8FD744FF",
                                 "#FDE725FF", "#D2E21BFF"),
    size = 3, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 25, nudge_y = 0, point.padding = .92, max.overlaps = Inf
  )+
  scale_color_viridis_d()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(colour = guide_legend(title.position = "top"))+
  theme(legend.position = "bottom",
        legend.key.height = unit(.6, 'cm'), #change legend key height
        legend.key.width = unit(.3, 'cm'),
        legend.key.size = unit(4.5, 'cm'))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 3")

ggsave("code/figures/rmse_experiment3.pdf", height = 4.67, width = 4)
