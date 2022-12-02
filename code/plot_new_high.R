library(ggplot2)
library(dplyr)
library(viridis)
library(reshape2)
library(scales)
library(ggrepel)
library(cowplot)
library(ggpubr)


mse_table <- readRDS(file = "code/new_high_mse.RDS")
sd_table <- readRDS(file = "code/new_high_sd.RDS")

rmse_table <- mse_table
rmse_table[,-c(1:4)] <- sqrt(rmse_table[,-c(1:4)])

rmse_table <- rmse_table[order(rmse_table$`Exp#`),]
rmse_table <- rmse_table[order(rmse_table$`Exp#`, rmse_table$P, rmse_table$K, rmse_table$N),]

linetypes <- c(
               "LASSO" = "dotted",
               "HT" = "solid",
               "CTMLE" = "dashed"
)

colors <- c(
            "LASSO" = "#AA4499",
            "HT" = "#117733",
            "CTMLE" = "#661100"
)

col2 <- c(
          "#AA4499",
          "#117733",
          "#661100")


for (i in 1:nrow(rmse_table)) {
  k =rmse_table$K[i]
  p = rmse_table$P[i]
  exp = rmse_table$`Exp#`[i]

  titlename <- paste0("Experiment ",exp,": K = ",k,", P = ",p)
  filename <- paste0("code/figures/new_high_rmse_experiment",exp,"k",k,"p",10*p,".pdf")

  rmse_table %>%
    filter(K == k, P == p,`Exp#` == exp) %>%
    ungroup() %>%
    dplyr::select(-`K`, -`Exp#`,-P) %>%
    melt(id.vars = "N") %>%
    dplyr::rename(Estimator = variable) %>%
    dplyr::filter(N == 1e4) -> end_values

  end_values$Color = c("#661100",
                       "#AA4499",
                       "#117733")
  sd_table %>%
    filter(K == k, P == p,`Exp#` == exp) %>%
    ungroup() %>%
    dplyr::select(-`K`, -`Exp#`,-P) %>%
    melt(id = "N") %>%
    dplyr::select(value) %>%
    dplyr::rename(sd = value) -> sd_values


  rmse_table %>%
    filter(K == k, P == p,`Exp#` == exp) %>%
    ungroup() %>%
    dplyr::select(-`K`, -`Exp#`,-P) %>%
    melt(id = "N") -> data_tab

  data.frame(data_tab, sd = sd_values) %>%
    dplyr::rename(Estimator = variable) %>%
    ggplot(aes(x = N, y = value, color = Estimator, linetype = Estimator))+
    geom_line(show.legend = FALSE)+
    geom_errorbar(aes(ymin=value-1.96*sd, ymax=value+1.96*sd), position=position_dodge(.9),show.legend = FALSE)+
    scale_linetype_manual(values = linetypes)+
    scale_color_manual(values = colors)+
    xlim(0, 11000) +
    geom_text_repel(
      aes(label = Estimator),
      data = end_values, color = end_values$Color,
      size = 2, force = 3,arrow = arrow(length = unit(0.01, "npc")),
      direction = "both", nudge_x = 500, nudge_y = .005, point.padding = .72, max.overlaps = Inf
    )+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(y = "RMSE when estimating ATE", x = "Sample Size", title = titlename)

  ggsave(filename = filename, height = 4, width = 4)

}
