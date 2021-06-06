library(ggplot2)
library(dplyr)
library(viridis)

load("res.RDS")

# Plot Experiment 1 ------------------------------------------------------------
res %>%
  filter(Exp == 1) %>%
  dplyr::select(-Exp) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1" = "Nearest Neighbor Matching (M=1)",
                                                       "ht" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line()+
  scale_color_viridis_d()+
  theme_bw()+
  geom_hline(aes(yintercept=.25))+
  labs(y = "Predicted ATE", x = "Sample Size")

ggsave("figures/experiment1.pdf",height = 6, width = 6)

# Plot Experiment 2 ------------------------------------------------------------
res %>%
  filter(Exp == 2) %>%
  dplyr::select(-Exp) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1" = "Nearest Neighbor Matching (M=1)",
                                                       "ht" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line()+
  scale_color_viridis_d()+
  theme_bw()+
  geom_hline(aes(yintercept=.2058))+
  labs(y = "Predicted ATE", x = "Sample Size")

ggsave("figures/experiment2.pdf",height = 6, width = 6)

# Plot Experiment 3 ------------------------------------------------------------
res %>%
  filter(Exp == 3) %>%
  dplyr::select(-Exp) %>%
  melt(id = "N") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("nn1" = "Nearest Neighbor Matching (M=1)",
                                                       "ht" = "Horvitz-Thompson"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line()+
  scale_color_viridis_d()+
  theme_bw()+
  geom_hline(aes(yintercept=.25))+
  labs(y = "Predicted ATE", x = "Sample Size")

ggsave("figures/experiment3.pdf",height = 6, width = 6)

