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

for (file in dir("code/results_12_13")) {
  print(file)
  if (substr(file, 1,3) == "res") {
    data <- readRDS(paste0("code/results_12_13/",file))
    all_data <- rbind(all_data, data)
  }
}

for (file in dir("code/results_exp5")) {
  print(file)
  if (substr(file, 1,3) == "res") {
    data <- readRDS(paste0("code/results_exp5/",file))
    all_data <- rbind(all_data, data)
  }
}


# all_data %>%
#   filter(N %in% c(1e3,1e4,1e5)) -> all_data

# Add the true average treatment effects for the different experiments ---------
all_data$TrueATE <- ifelse(all_data$Exp==1,.5861,
                           ifelse(all_data$Exp==2, .32346, .5))


all_data %>%
  dplyr::mutate(across(-c(1,2), ~  . - TrueATE)) %>%
  dplyr::mutate(across(-c(1,2), ~  . **2)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,Exp) %>%
  summarise(across(everything(), list(mean))) -> mse_table

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
