library(ggplot2)
library(dplyr)
library(viridis)

all_data <- data.frame()


for (file in dir("code/results_end")) {
  print(file)
  if (substr(file, 1,3) == "res") {
    data <- readRDS(paste0("code/results_end/",file))
    all_data <- rbind(all_data, data)
  }
  if (substr(file, 1,4) == "last") {
    data <- readRDS(paste0("code/results_end/",file))
    all_data <- rbind(all_data, data)
  }
}


#all_data[order(all_data$Exp),]

# Read in ATE's
ate3 <- readRDS("~/Desktop/identification-not-enough/code/ate3.RDS")
ate2 <- readRDS("~/Desktop/identification-not-enough/code/ate2.RDS")
ate1 <- readRDS("~/Desktop/identification-not-enough/code/ate1.RDS")

all_data$TrueATE <- NA

ate1$Exp = 1
ate2$Exp = 2
ate3$Exp = 3

all_data$N <- as.numeric(all_data$N)
all_data$P <- as.numeric(all_data$P)
all_data$K <- as.numeric(all_data$K)
all_data$Exp <- as.numeric(all_data$Exp)

# Fill true ATE's
all_data <- dplyr::left_join(all_data, ate1, by = c("N","K","P","Exp"))
all_data <- dplyr::left_join(all_data, ate2, by = c("N","K","P","Exp"))
all_data <- dplyr::left_join(all_data, ate3, by = c("N","K","P","Exp"))

all_data$TrueATE <- ifelse(!is.na(all_data$ATE.x), all_data$ATE.x,
                           ifelse(!is.na(all_data$ATE.y), all_data$ATE.y,
                                  ifelse(!is.na(all_data$ATE), all_data$ATE, NA)))

all_data <- all_data %>% dplyr::select(-ATE,-ATE.x,-ATE.y)

for (i in 1:ncol(all_data)) {
  all_data[,i] <- as.numeric(all_data[,i])
}

all_data <- all_data[complete.cases(all_data),]

# all_data[rowSums(is.na(all_data)) > 0,]


# Now read in the re run data for CTMLE
new_data <- data.frame()
for (file in dir("code/results_3_26")) {
  print(file)
  if (substr(file, 1,3) == "res") {
    data <- readRDS(paste0("code/results_3_26/",file))
    new_data <- rbind(new_data, data)
  }
}

new_data %>%
  group_by(N,K,P,Exp) %>%
  summarise(count = n()) %>%
  arrange(count)


new_data$TrueATE <- NA

ate1$Exp = 1
ate2$Exp = 2
ate3$Exp = 3

new_data$N <- as.numeric(new_data$N)
new_data$P <- as.numeric(new_data$P)
new_data$K <- as.numeric(new_data$K)
new_data$Exp <- as.numeric(new_data$Exp)

# Fill true ATE's
new_data <- dplyr::left_join(new_data, ate1, by = c("N","K","P","Exp"))
new_data <- dplyr::left_join(new_data, ate2, by = c("N","K","P","Exp"))
new_data <- dplyr::left_join(new_data, ate3, by = c("N","K","P","Exp"))

new_data$TrueATE <- ifelse(!is.na(new_data$ATE.x), new_data$ATE.x,
                           ifelse(!is.na(new_data$ATE.y), new_data$ATE.y,
                                  ifelse(!is.na(new_data$ATE), new_data$ATE, NA)))

new_data <- new_data %>% dplyr::select(-ATE,-ATE.x,-ATE.y)

for (i in 1:ncol(new_data)) {
  new_data[,i] <- as.numeric(new_data[,i])
}
new_data <- new_data[complete.cases(new_data),]
# new_data[rowSums(is.na(new_data)) > 0,]


new_data %>%
  dplyr::filter(!is.na(tmle_c)) %>%
  dplyr::mutate(across(-c(1:4), ~  . - TrueATE)) %>%
  dplyr::mutate(across(-c(1:4), ~  . **2)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,K,P,Exp) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE, trim = .025))) -> new_mse_table

new_data %>%
  dplyr::filter(!is.na(tmle_c)) %>%
  dplyr::mutate(across(-c(1:4), ~  . - TrueATE)) %>%
  dplyr::mutate(across(-c(1:4), ~  . **2)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,K,P,Exp) %>%
  summarise(across(everything(), ~ sd(.x) / sqrt(n()) )) -> new_sd_table

new_data %>%
  dplyr::filter(!is.na(tmle_c)) %>%
  dplyr::mutate(across(-c(1:4), ~  . - TrueATE)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,K,P,Exp) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) -> new_bias_table

new_var_table <- new_mse_table[,-c(1:4)] - (new_bias_table[,-c(1:4)])**2
new_se_table <- sqrt(abs(new_var_table))
new_se_table <- cbind(new_mse_table[,c(1:4)], new_se_table)

colnames(new_mse_table) <- c("N","K","P","Exp#","CTMLE",
                         "LASSO", "HT")


colnames(new_bias_table) <- c("N","K","P","Exp#","CTMLE",
                              "LASSO", "HT")


colnames(new_se_table) <- c("N","K","P","Exp#","CTMLE",
                            "LASSO", "HT")
colnames(new_sd_table) <- c("N","K","P","Exp#","CTMLE",
                            "LASSO", "HT")

saveRDS(new_mse_table, file = "code/new_high_mse_truncated.RDS")
saveRDS(new_sd_table, file = "code/new_high_sd.RDS")
saveRDS(new_bias_table, file = "code/new_high_bias.RDS")
saveRDS(new_se_table, file = "code/new_high_se.RDS")


# Old SE + Bias + variance tables

all_data %>%
  dplyr::filter(!is.na(tmle_c)) %>%
  dplyr::mutate(across(-c(1:4), ~  . - TrueATE)) %>%
  dplyr::mutate(across(-c(1:4), ~  . **2)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,K,P,Exp) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE, trim = .025))) -> mse_table

all_data %>%
  dplyr::filter(!is.na(tmle_c)) %>%
  dplyr::mutate(across(-c(1:4), ~  . - TrueATE)) %>%
  dplyr::mutate(across(-c(1:4), ~  . **2)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,K,P,Exp) %>%
  summarise(across(everything(), ~ sd(.x) / sqrt(n()) )) -> sd_table

all_data %>%
  dplyr::filter(!is.na(tmle_c)) %>%
  dplyr::mutate(across(-c(1:4), ~  . - TrueATE)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,K,P,Exp) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) -> bias_table


var_table <- mse_table[,-c(1:4)] - (bias_table[,-c(1:4)])**2
se_table <- sqrt(abs(var_table))
se_table <- cbind(mse_table[,c(1:4)], se_table)

mse_table <- mse_table #%>% dplyr::select(-dr_lasso_cf_1,-ht_lasso_cf_1)

colnames(mse_table) <- c("N","K","P","Exp#","TMLE","CTMLE","NN Matching",
                         "LASSO", "DR (lasso)", "HT (lasso)","HT", "PS Matching")


colnames(bias_table) <- c("N","K","P","Exp#","TMLE","CTMLE","NN Matching",
                         "LASSO", "DR (lasso)", "HT (lasso)","HT", "PS Matching")


colnames(se_table) <- c("N","K","P","Exp#","TMLE","CTMLE","NN Matching",
                         "LASSO", "DR (lasso)", "HT (lasso)","HT", "PS Matching")
colnames(sd_table) <- c("N","K","P","Exp#","TMLE","CTMLE","NN Matching",
                        "LASSO", "DR (lasso)", "HT (lasso)","HT", "PS Matching")

library(xtable)

saveRDS(mse_table, file = "code/high_mse_truncated.RDS")
saveRDS(sd_table, file = "code/high_sd.RDS")
saveRDS(bias_table, file = "code/high_bias.RDS")
saveRDS(se_table, file = "code/high_se.RDS")

rmse_table <- mse_table
rmse_table[,-c(1:4)] <- sqrt(rmse_table[,-c(1:4)])

rmse_table <- rmse_table[order(rmse_table$`Exp#`),]
rmse_table <- rmse_table[order(rmse_table$`Exp#`, rmse_table$P, rmse_table$K, rmse_table$N),]

xtable(rmse_table, digits = 4, caption = "Results for the RMSE in high dimensional simulations")





all_data %>%
  dplyr::filter(!is.na(tmle_c)) %>%
  dplyr::mutate(across(-c(1:4), ~  . - TrueATE)) -> te

abs(te) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,K,P,Exp) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) -> mae_table

colnames(mae_table) <- c("N","K","P","Exp#","TMLE","CTMLE","NN Matching",
                         "LASSO", "DR (lasso)", "HT (lasso)","HT", "PS Matching")

saveRDS(mae_table, file = "code/high_mae.RDS")
