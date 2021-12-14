library(ggplot2)
library(dplyr)
library(viridis)

all_data <- data.frame()

for (file in dir("code/results")) {
  print(file)
  if (substr(file, 1,3) == "new") {
    data <- readRDS(paste0("code/results/",file))
    all_data <- rbind(all_data, data)
  }
}

for (file in dir("code/res_newer")) {
  print(file)
  if (substr(file, 1,5) == "final") {
    data <- readRDS(paste0("code/res_newer/",file))
    all_data <- rbind(all_data, data)
  }
}

for (file in dir("code/results_tmle")) {
  print(file)
  if (substr(file, 1,3) == "new") {
    data <- readRDS(paste0("code/results_tmle/",file))
    all_data <- rbind(all_data, data)
  }
}

for (file in dir("code/results_ctmle")) {
  print(file)
  if (substr(file, 1,3) == "new") {
    data <- readRDS(paste0("code/results_ctmle/",file))
    all_data <- rbind(all_data, data)
  }
}

for (file in dir("code/results_high")) {
  print(file)
  if (substr(file, 1,3) == "res") {
    data <- readRDS(paste0("code/results_high/",file))
    all_data <- rbind(all_data, data)
  }
}

for (file in dir("code/results_12_13")) {
  print(file)
  if (substr(file, 1,3) == "res") {
    data <- readRDS(paste0("code/results_12_13/",file))
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


all_data %>%
  dplyr::filter(!is.na(tmle_c)) %>%
  dplyr::mutate(across(-c(1:4), ~  . - TrueATE)) %>%
  dplyr::mutate(across(-c(1:4), ~  . **2)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,K,P,Exp) %>%
  summarise(across(everything(), list(mean))) -> mse_table

mse_table <- mse_table #%>% dplyr::select(-dr_lasso_cf_1,-ht_lasso_cf_1)

colnames(mse_table) <- c("N","K","P","Exp#","TMLE","CTMLE","NN Matching",
                         "LASSO", "DR (lasso)", "HT (lasso)","HT", "PS Matching")

library(xtable)

rmse_table <- mse_table
rmse_table[,-c(1:4)] <- sqrt(rmse_table[,-c(1:4)])

rmse_table <- rmse_table[order(rmse_table$`Exp#`),]
rmse_table <- rmse_table[order(rmse_table$`Exp#`, rmse_table$P, rmse_table$K, rmse_table$N),]

xtable(rmse_table, digits = 4, caption = "Partial results for the RMSE in high dimensional simulations")

# all_data %>%
#   filter(N %in% c(1e3,1e4,1e5)) -> all_data

# Add the true average treatment effects for the different experiments ---------
all_data$TrueATE <- ifelse(all_data$Exp==1,.5861,
                           ifelse(all_data$Exp==2, .32346, .5))


all_data %>%
  dplyr::filter(!is.na) %>%
  dplyr::mutate(across(-c(1,2), ~  . - TrueATE)) %>%
  dplyr::mutate(across(-c(1,2), ~  . **2)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,Exp) %>%
  summarise(across(everything(), list(mean))) -> mse_table



write.csv(mse_table, file = "code/ctmleMSEtable.csv")

# For some reason RF failed on one run of experiments, exclude these for now ---
# all_data[rowSums(is.na(all_data)) > 0,]
all_data %>%
  filter(!is.na(rf)) %>%
  filter(!is.na(loop_rf)) %>%
  filter(!is.na(adjusted_ht))-> all_data


# Now get Bias -----------------------------------------------------------------
all_data %>%
  dplyr::mutate(across(-c(1,2), ~  . - TrueATE)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,Exp) %>%
  summarise(across(everything(), list(mean))) -> bias_table


write.csv(bias_table, file = "code/BIAStable.csv")

# Now get MSE ------------------------------------------------------------------

all_data %>%
  dplyr::mutate(across(-c(1,2), ~  . - TrueATE)) %>%
  dplyr::mutate(across(-c(1,2), ~  . **2)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,Exp) %>%
  summarise(across(everything(), list(mean))) -> mse_table

write.csv(mse_table, file = "code/finalMSEtable.csv")


# Get variance -----------------------------------------------------------------
mse_table[,-c(1,2)] - bias_table[,-c(1,2)]**2 -> var_table
var_table <- cbind(mse_table[,c(1,2)],var_table)

# See if any are Nan
var_table[rowSums(is.na(var_table)) > 0,]

write.csv(var_table, file = "code/VARtable.csv")
