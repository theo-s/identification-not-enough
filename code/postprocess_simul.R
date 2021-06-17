library(ggplot2)
library(dplyr)
library(viridis)

all_data <- data.frame()

for (file in dir("code/results")) {
  print(file)
  data <- readRDS(paste0("code/results/",file))
  all_data <- rbind(all_data, data)
}

all_data %>%
  filter(N %in% c(1e3,1e4,1e5)) -> all_data

# Add the true average treatment effects for the different experiments ---------
all_data$TrueATE <- ifelse(all_data$Exp==1,.5861,
                           ifelse(all_data$Exp==2, .32346, .5))


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

write.csv(mse_table, file = "code/MSEtable.csv")


# Get variance -----------------------------------------------------------------
mse_table[,-c(1,2)] - bias_table[,-c(1,2)]**2 -> var_table
var_table <- cbind(mse_table[,c(1,2)],var_table)

# See if any are Nan
var_table[rowSums(is.na(var_table)) > 0,]

write.csv(var_table, file = "code/VARtable.csv")
