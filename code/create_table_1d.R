library(ggplot2)
library(dplyr)
library(viridis)
library(reshape)
library(scales)
library(ggrepel)
library(cowplot)
library(ggpubr)
library(xtable)

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

tmle_mse_table <- read.csv(file = "code/tmleMSEtable.csv")
tmle_rmse_table <- tmle_mse_table
tmle_rmse_table[,-c(1:3)] <- sqrt(tmle_rmse_table[,-c(1:3)])

ctmle_mse_table <- read.csv(file = "code/ctmleMSEtable.csv")
ctmle_rmse_table <- ctmle_mse_table
ctmle_rmse_table[,-c(1:3)] <- sqrt(ctmle_rmse_table[,-c(1:3)])

rmse_table <- cbind(rmse_table,
                    new_rmse_table[,c(4,5)],
                    final_rmse_table[,c(5,6,8)],
                    tmle_rmse_table[,c(4,5)],
                    ctmle = ctmle_rmse_table[,4])

names <- c("X","N","Exp","NN Matching","Horvitz-Thompson","LOO RF","PS Matching (True)",
           "Logistic", "RF", "DR Logit","CF RF","DRRF", "DRRF CF","HT RF","TMLE SL",
           "TMLE HAL", "CTMLE")

ordering <- c("X","N","Exp","NN Matching",
               "Logistic",
               "RF",
               "DR Logit",
               "DRRF",
               "DRRF CF" ,
               "Horvitz-Thompson",
               "LOO RF",
               "PS Matching (True)",
               "CF RF",
               "HT RF",
               "TMLE SL",
               "TMLE HAL",
               "CTMLE")

rmse_table %>%
  dplyr::select(-adjusted_ht_1,-ps_rf1_1,-ps_logit1_1) %>%
  setNames(names) %>%
  dplyr::select(X,N,Exp,`NN Matching`,`Logistic`,`RF`,`DR Logit`,`DRRF`,`DRRF CF` ,
                `Horvitz-Thompson`,`LOO RF`,`PS Matching (True)`,`CF RF`,
                `HT RF`,`TMLE SL`,`TMLE HAL`,`CTMLE`) %>%
  dplyr::select(-X) %>%
  dplyr::arrange(Exp,N) -> tab

xtable(tab, digits = 4, caption = "Tabular results of the 1-dimensional simulation results")

