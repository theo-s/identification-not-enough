library(ggplot2)
library(dplyr)
library(viridis)
library(reshape)
library(scales)
library(ggrepel)
library(cowplot)
library(kableExtra)
library(magick)
library(ggpubr)

# Plot Bias --------------------------------------------------------------------

# Set linetypes for different estimator styles
linetypes <- c(
               "Horvitz-Thompson" = "solid",
               "LOO RF " = "solid",
               "PS Matching (True)" = "solid",
               "CF RF" = "solid",
               "HT RF" = "solid"
)

# scales::show_col(safe_colorblind_palette)
colors <- c(
            "Horvitz-Thompson" = "#6699CC",
            "LOO RF " = "#332288",
            "PS Matching (True)" = "#CC6677",
            "CF RF" = "#117733",
            "HT RF" = "#661100"
)

col2 <- c("#CC6677",
          "#661100",
          "#332288",
          "#117733",
          "#6699CC")

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

# Plot Experiment 1 ------------------------------------------------------------
rmse_table %>%
  dplyr::filter(Exp == 1) %>%
  dplyr::select(-X, -Exp) %>%
  dplyr::select(N,ps1_1, dr_noCF_1,loop_rf_1,cross_fit_1,ht_1) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("ps1_1" = "PS Matching (True)",
                                                       "dr_noCF_1" = "HT RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 1e5) -> end_values

rmse_table %>%
  filter(Exp == 1) %>%
  dplyr::filter(N > 100) %>%
  dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-Exp, -X) %>%
  dplyr::select(N,ps1_1, dr_noCF_1,loop_rf_1,cross_fit_1,ht_1) %>%
  dplyr::filter(N > 1e3) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("ps1_1" = "PS Matching (True)",
                                                       "dr_noCF_1" = "HT RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  xlim(0, 140000) +
  ylim(0,.011)+
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = col2,
    size = 2, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 5000, nudge_y = 0, point.padding = .72, max.overlaps = Inf
  )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 1")

ggsave("code/figures/new_rmse_experiment1.pdf", height = 4, width = 4)

# Plot Experiment 2 ------------------------------------------------------------
rmse_table %>%
  dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::filter(Exp == 2) %>%
  dplyr::select(-X, -Exp) %>%
  dplyr::select(N,ps1_1, dr_noCF_1,loop_rf_1,cross_fit_1,ht_1) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("ps1_1" = "PS Matching (True)",
                                                       "dr_noCF_1" = "HT RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 1e5) -> end_values

rmse_table %>%
  filter(Exp == 2) %>%
  dplyr::filter(N > 100) %>%
  dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-Exp, -X) %>%
  dplyr::select(N,ps1_1, dr_noCF_1,loop_rf_1,cross_fit_1,ht_1) %>%
  dplyr::filter(N > 1e3) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("ps1_1" = "PS Matching (True)",
                                                       "dr_noCF_1" = "HT RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  xlim(0, 140000) +
  ylim(0,.015)+
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = col2,
    size = 2, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 5000, nudge_y = 0, point.padding = .92, max.overlaps = Inf
  )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 2")

ggsave("code/figures/new_rmse_experiment2.pdf", height = 4, width = 4)


# Plot Experiment 3 ------------------------------------------------------------
rmse_table %>%
  dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::filter(Exp == 3) %>%
  dplyr::select(-X, -Exp) %>%
  dplyr::select(N,ps1_1, dr_noCF_1,loop_rf_1,cross_fit_1,ht_1) %>%
  melt(id.vars = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("ps1_1" = "PS Matching (True)",
                                                       "dr_noCF_1" = "HT RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  dplyr::filter(N == 1e5) -> end_values

rmse_table %>%
  filter(Exp == 3) %>%
  dplyr::filter(N > 100) %>%
  dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  dplyr::select(-Exp, -X) %>%
  dplyr::select(N,ps1_1, dr_noCF_1,loop_rf_1,cross_fit_1,ht_1) %>%
  dplyr::filter(N > 1e3) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("ps1_1" = "PS Matching (True)",
                                                       "dr_noCF_1" = "HT RF",
                                                       "loop_rf_1" = "LOO RF ",
                                                       "cross_fit_1" = "CF RF",
                                                       "ht_1" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
  geom_line(show.legend = FALSE)+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  xlim(0, 140000) +
  ylim(0,.015)+
  geom_text_repel(
    aes(label = Estimator),
    data = end_values, color = col2,
    size = 2, force = 3,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 5000, nudge_y = 0, point.padding = .52, max.overlaps = Inf
  )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  guides(colour = guide_legend(title.position = "top"))+
  theme(legend.position = "bottom",
        legend.key.height = unit(.6, 'cm'), #change legend key height
        legend.key.width = unit(.3, 'cm'),
        legend.key.size = unit(4.5, 'cm'))+
  labs(y = "RMSE when estimating ATE", x = "Sample Size", title = "Experiment 3")


ggsave("code/figures/new_rmse_experiment3.pdf", height = 4, width = 4)


# Make a table of the RMSE values
all_data<-data.frame()
for (expnm in c(1:3)) {
  rmse_table %>%
    filter(Exp == expnm) %>%
    dplyr::filter(N > 100) %>%
    dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
    dplyr::select(-Exp, -X) %>%
    dplyr::select(N,ps1_1, dr_noCF_1,loop_rf_1,cross_fit_1,ht_1) -> table
    all_data <- rbind(all_data,table)
}
colnames(all_data) <- c("N", "PS Matching (True)","HT RF","LOO RF", "CF RF","Horvitz-Thompson")

all_data <- format(all_data, scientific = TRUE, digits = 3)

all_data %>%
  kbl(booktabs = TRUE) %>%
  kable_paper("striped", full_width = F) %>%
  column_spec(1, border_left = "2px solid black") %>%
  column_spec(6, border_right = "2px solid black") %>%
  pack_rows("Experiment 1", 1, 3) %>%
  pack_rows("Experiment 2", 4, 6) %>%
  pack_rows("Experiment 3", 7, 9) %>%
  save_kable("code/figures/rmse_table.pdf",density = 1000)



