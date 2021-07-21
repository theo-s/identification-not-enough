library(ggplot2)
library(dplyr)
library(viridis)
library(reshape)
library(scales)
library(ggrepel)
library(cowplot)
library(ggpubr)

# Plot Bias --------------------------------------------------------------------
# bias_table <- read.csv(file = "code/BIAStable.csv")

# Set linetypes for different estimator styles
linetypes <- c("NN Matching" = "dotted",
               "Logistic" = "dotted",
               "RF" = "dotted",
               "DR Logit" = "dashed",
               "DRRF" = "dashed",
               "DRRF CF" = "dashed",
               "Horvitz-Thompson" = "solid",
               "LOO RF " = "solid",
               "PS Matching (True)" = "solid",
               "CF RF" = "solid",
               "HT RF" = "solid"
)

# scales::show_col(safe_colorblind_palette)
colors <- c("NN Matching" = "#88CCEE",
            "Logistic" = "#AA4499",
            "RF" = "#44AA99",
            "DR Logit" = "#999933",
            "DRRF" = "#DDCC77",
            "DRRF CF" = "#888888",
            "Horvitz-Thompson" = "#6699CC",
            "LOO RF " = "#332288",
            "PS Matching (True)" = "#CC6677",
            "CF RF" = "#117733",
            "HT RF" = "#661100"
)

col2 <- c("#88CCEE",
          "#6699CC",
          "#661100",
          "#332288",
          "#CC6677",
          "#DDCC77",
          "#888888",
          "#AA4499",
          "#44AA99",
          "#999933",
          "#117733")

# Plot sqrt(var) --------------------------------------------------------------------
mse_table <- read.csv(file = "code/MSEtable.csv")
rmse_table <- mse_table
rmse_table[,-c(1:3)] <- sqrt(mse_table[,-c(1:3)])

new_mse_table <- read.csv(file = "code/newMSEtable.csv")
new_rmse_table <- new_mse_table
new_rmse_table[,-c(1:3)] <- sqrt(new_mse_table[,-c(1:3)])

rmse_table <- cbind(rmse_table, new_rmse_table[,c(4,5)])

# Plot Experiment 1 ------------------------------------------------------------
rmse_table %>%
  dplyr::filter(Exp == 1) %>%
  #dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-X, -Exp) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "PS Matching (True)",
                                                       "ps_rf1_1" = "DRRF",
                                                       "ps_logit1_1" = "DRRF CF",
                                                       "adjusted_ht_1" = "HT RF",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "dr_logit_1" = "DR Logit",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 1e5) -> end_values

rmse_table %>%
  filter(Exp == 1) %>%
  dplyr::filter(N > 100) %>%
  #dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-Exp, -X) %>%
  dplyr::select(N,nn1_1, lr_1,rf_1,dr_logit_1,ht_1,loop_rf_1,ps1_1,cross_fit_1,adjusted_ht_1,ps_rf1_1,ps_logit1_1) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "PS Matching (True)",
                                                       "ps_rf1_1" = "DRRF",
                                                       "ps_logit1_1" = "DRRF CF",
                                                       "adjusted_ht_1" = "HT RF",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "dr_logit_1" = "DR Logit",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  xlim(0, 110000) +
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = col2,
    size = 2, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 500, nudge_y = .005, point.padding = .72, max.overlaps = Inf
  )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 1")

ggsave("code/figures/rmse_experiment1.pdf", height = 4, width = 4)

# Plot Experiment 2 ------------------------------------------------------------
rmse_table %>%
  #dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::filter(Exp == 2) %>%
  dplyr::select(-X, -Exp) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "PS Matching (True)",
                                                       "ps_rf1_1" = "DRRF",
                                                       "ps_logit1_1" = "DRRF CF",
                                                       "adjusted_ht_1" = "HT RF",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "dr_logit_1" = "DR Logit",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 1e5) -> end_values

rmse_table %>%
  filter(Exp == 2) %>%
  dplyr::filter(N > 100) %>%
  #dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-Exp, -X) %>%
  dplyr::select(N,nn1_1, lr_1,rf_1,dr_logit_1,ht_1,loop_rf_1,ps1_1,cross_fit_1,adjusted_ht_1,ps_rf1_1,ps_logit1_1) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "PS Matching (True)",
                                                       "ps_rf1_1" = "DRRF",
                                                       "ps_logit1_1" = "DRRF CF",
                                                       "adjusted_ht_1" = "HT RF",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "dr_logit_1" = "DR Logit",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  xlim(0, 110000) +
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = col2,
    size = 2, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 1000, nudge_y = .02, point.padding = .92, max.overlaps = Inf
  )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 2")

ggsave("code/figures/rmse_experiment2.pdf", height = 4, width = 4)


# Plot Experiment 3 ------------------------------------------------------------
rmse_table %>%
  #dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::filter(Exp == 3) %>%
  dplyr::select(-X, -Exp) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "PS Matching (True)",
                                                       "ps_rf1_1" = "DRRF",
                                                       "ps_logit1_1" = "DRRF CF",
                                                       "adjusted_ht_1" = "HT RF",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "dr_logit_1" = "DR Logit",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 1e5) -> end_values

rmse_table %>%
  filter(Exp == 3) %>%
  dplyr::filter(N > 100) %>%
  #dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-Exp, -X) %>%
  dplyr::select(N,nn1_1, lr_1,rf_1,dr_logit_1,ht_1,loop_rf_1,ps1_1,cross_fit_1,adjusted_ht_1,ps_rf1_1,ps_logit1_1) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "PS Matching (True)",
                                                       "ps_rf1_1" = "DRRF",
                                                       "ps_logit1_1" = "DRRF CF",
                                                       "adjusted_ht_1" = "HT RF",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "dr_logit_1" = "DR Logit",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  xlim(0, 110000) +
  ylim(0,.35)+
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = col2,
    size = 2, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 500, nudge_y = .05, point.padding = .52, max.overlaps = Inf
  )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(colour = guide_legend(title.position = "top"))+
  theme(legend.position = "bottom",
        legend.key.height = unit(.6, 'cm'), #change legend key height
        legend.key.width = unit(.3, 'cm'),
        legend.key.size = unit(4.5, 'cm'))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 3")


ggsave("code/figures/rmse_experiment3.pdf", height = 4, width = 4)

# Plot the legend ==============================================================
rmse_table %>%
  filter(Exp == 3) %>%
  dplyr::filter(N > 100) %>%
  #dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-Exp, -X) %>%
  dplyr::select(N,nn1_1, lr_1,rf_1,dr_logit_1,ps_rf1_1,ps_logit1_1,ht_1,loop_rf_1,ps1_1,cross_fit_1,adjusted_ht_1) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NN Matching",
                                                       "ps1_1" = "PS Matching (True)",
                                                       "ps_rf1_1" = "DRRF",
                                                       "ps_logit1_1" = "DRRF CF",
                                                       "adjusted_ht_1" = "HT RF",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "dr_logit_1" = "DR Logit",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
  geom_line(show.legend = TRUE, size = 2)+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  xlim(0, 110000) +
  ylim(0,.35)+
  geom_text_repel(
    aes(label = Estimator, color = Estimator),
    data = end_values, color = col2,
    size = 2, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 500, nudge_y = .05, point.padding = .52, max.overlaps = Inf
  )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(colour = guide_legend(title.position = "top"))+
  theme(legend.position = "left",
        legend.key.height = unit(.6, 'cm'), #change legend key height
        legend.key.width = unit(3, 'cm'),
        legend.spacing.x = unit(.1, 'cm'),
        legend.spacing.y = unit(.02, 'cm'),
        legend.key.size = unit(8, 'cm'),
        legend.title.align = .5)+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 3") -> p3

legend <- cowplot::get_legend(p3)
as_ggplot(legend)

ggsave("code/figures/legend.pdf", height = 3.5, width = 2.5)


library(kableExtra)
library(magick)


names <- data.frame(Estimator = c( "Nearest Neighbor Matching",
                                   "Linear Logistic Regression",
                                   "Random Forest",
                                   "Double Robust Estimator using Linear Logistic Regression",
                                   "Double Robust Estimator using Random Forest",
                                   "Double Robust Estimator using Random Forest (cross-fitting)",
                                   "Standard Horvitz-Thompson Estimator",
                                   "Leave-one-out Random Forest Adjusted Horvitz-Thompson Estimator",
                                   "Propensity Score Matching on True Propensity Score",
                                   "Cross-fitting Horvitz-Thompson Estimator with RF for Covariate Adjustment",
                                   "Horvitz-Thompson Estimator with RF for Covariate Adjustment"),
                    Shorthand = c("NN Matching",
                                  "Logistic",
                                  "RF",
                                  "DR Logit",
                                  "DRRF",
                                  "DRRF CF",
                                  "Horvitz-Thompson",
                                  "LOO RF",
                                  "PS Matching (True)",
                                  "CF RF",
                                  "HT RF"),
                    Line = "")

names %>%
  kbl(booktabs = TRUE) %>%
  kable_paper("striped", full_width = F) %>%
  column_spec(1, border_left = "2px solid black") %>%
  column_spec(2, border_right = "2px solid black") %>%
  column_spec(3, background = colors) %>%
  pack_rows("No Propensity Score Used", 1, 3) %>%
  pack_rows("Estimated Propensity Score Used", 4, 6) %>%
  pack_rows("True Propensity Score Used", 7, 11) %>%
  save_kable("code/figures/table.pdf",density = 1000)


