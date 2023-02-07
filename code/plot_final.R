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
linetypes <- c("NNM" = "dotted",
               "Logistic" = "dotted",
               "RF" = "dotted",
               "DR Logit" = "dashed",
               "DRRF" = "dashed",
               "DRRF CF" = "dashed",
               "HT" = "solid",
               "LOO RF " = "solid",
               "PSM (True)" = "solid",
               "CF RF" = "solid",
               "HT RF" = "solid"
)

# scales::show_col(safe_colorblind_palette)
colors <- c("NNM" = "#88CCEE",
            "Logistic" = "#AA4499",
            "RF" = "#44AA99",
            "DR Logit" = "#999933",
            "DRRF" = "#DDCC77",
            "DRRF CF" = "#888888",
            "HT" = "#6699CC",
            "LOO RF " = "#332288",
            "PSM (True)" = "#CC6677",
            "CF RF" = "#117733",
            "HT RF" = "#661100"
)

col2 <- c("#88CCEE",
          "#6699CC",
          "#332288",
          "#CC6677",
          "#AA4499",
          "#44AA99",
          "#999933",
          "#117733",
          "#DDCC77",
          "#888888",
          "#661100")

# Plot sqrt(var) --------------------------------------------------------------------
mse_table <- read.csv(file = "code/MSEtable.csv")
rmse_table <- mse_table
rmse_table[,-c(1:3)] <- sqrt(mse_table[,-c(1:3)])

new_mse_table <- read.csv(file = "code/newMSEtable.csv")
new_rmse_table <- new_mse_table
new_rmse_table[,-c(1:3)] <- sqrt(new_mse_table[,-c(1:3)])

final_mse_table <- read.csv(file = "code/finalMSEtable.csv")
final_rmse_table <- final_mse_table
final_rmse_table[,-c(1:3)] <- sqrt(final_rmse_table[,-c(1:3)])


rmse_table <- cbind(rmse_table, new_rmse_table[,c(4,5)], final_rmse_table[,c(5,6,8)])

# Plot Data generating process 1 ------------------------------------------------------------
rmse_table %>%
  filter(Exp == 1) %>%
  dplyr::filter(N > 100) %>%
  dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-Exp, -X) %>%
  dplyr::select(N,nn1_1, lr_1,rf_1,dr_logit_1,ht_1,loop_rf_1,ps1_1,cross_fit_1,dr_noCF_1,dr_RF_Pscore_1,dr_RF_Pscore_CF_1) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NNM",
                                                       "ps1_1" = "PSM (True)",
                                                       "dr_RF_Pscore_1" = "DRRF",
                                                       "dr_RF_Pscore_CF_1" = "DRRF CF",
                                                       "dr_noCF_1" = "HT RF",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "dr_logit_1" = "DR Logit",
                                                       "ht_1" = "HT"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  xlim(0, 110000) +
  ylim(0,.04)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Data generating process 1") -> p1

# Plot Data generating process 2 ------------------------------------------------------------
rmse_table %>%
  filter(Exp == 2) %>%
  dplyr::filter(N > 100) %>%
  dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-Exp, -X) %>%
  dplyr::select(N,nn1_1, lr_1,rf_1,dr_logit_1,ht_1,loop_rf_1,ps1_1,cross_fit_1,dr_noCF_1,dr_RF_Pscore_1,dr_RF_Pscore_CF_1) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NNM",
                                                       "ps1_1" = "PSM (True)",
                                                       "dr_RF_Pscore_1" = "DRRF",
                                                       "dr_RF_Pscore_CF_1" = "DRRF CF",
                                                       "dr_noCF_1" = "HT RF",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "dr_logit_1" = "DR Logit",
                                                       "ht_1" = "HT"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  xlim(0, 110000) +
  ylim(0,.72)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Data generating process 2") -> p2


# Plot Data generating process 3 ------------------------------------------------------------
rmse_table %>%
  filter(Exp == 3) %>%
  dplyr::filter(N > 100) %>%
  dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-Exp, -X) %>%
  dplyr::select(N,nn1_1, lr_1,rf_1,dr_logit_1,ht_1,loop_rf_1,ps1_1,cross_fit_1,dr_noCF_1,dr_RF_Pscore_1,dr_RF_Pscore_CF_1) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NNM",
                                                       "ps1_1" = "PSM (True)",
                                                       "dr_RF_Pscore_1" = "DRRF",
                                                       "dr_RF_Pscore_CF_1" = "DRRF CF",
                                                       "dr_noCF_1" = "HT RF",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "dr_logit_1" = "DR Logit",
                                                       "ht_1" = "HT"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  xlim(0, 110000) +
  ylim(0,.35)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(colour = guide_legend(title.position = "top"))+
  theme(legend.position = "bottom",
        legend.key.height = unit(.6, 'cm'), #change legend key height
        legend.key.width = unit(.3, 'cm'),
        legend.key.size = unit(4.5, 'cm'))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Data generating process 3") -> p3


# Plot the legend ==============================================================
rmse_table %>%
  filter(Exp == 3) %>%
  dplyr::filter(N > 100) %>%
  dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-Exp, -X) %>%
  dplyr::select(N,nn1_1, lr_1,rf_1,dr_logit_1,dr_RF_Pscore_1,dr_RF_Pscore_CF_1,ht_1,loop_rf_1,ps1_1,cross_fit_1,dr_noCF_1) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1_1" = "NNM",
                                                       "ps1_1" = "PSM (True)",
                                                       "dr_RF_Pscore_1" = "DRRF",
                                                       "dr_RF_Pscore_CF_1" = "DRRF CF",
                                                       "dr_noCF_1" = "HT RF",
                                                       "lr_1" = "Logistic",
                                                       "rf_1" = "RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "dr_logit_1" = "DR Logit",
                                                       "ht_1" = "HT"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
  geom_line(show.legend = TRUE, size = 2)+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  xlim(0, 110000) +
  ylim(0,.35)+
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
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Data generating process 3") -> p4

legend <- cowplot::get_legend(p4)
leg_plot <- as_ggplot(legend)

ggarrange(plotlist = list(p1,p2,p3,legend),
          nrow = 2,
          ncol = 2
          )
ggsave(filename = "~/Desktop/identification-not-enough/code/figures/all_figs.pdf", width = 7,height = 6)

