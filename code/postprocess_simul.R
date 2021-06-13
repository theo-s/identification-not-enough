library(ggplot2)
library(dplyr)
library(viridis)

all_data <- data.frame()

for (file in dir("code/results")) {
  print(file)
  data <- readRDS(paste0("code/results/",file))
  all_data <- rbind(all_data, data)
}

# Add the true average treatment effects for the different experiments ---------
all_data$TrueATE <- ifelse(all_data$Exp==2,.2058,.25)


# For some reason RF failed on one run of experiments, exclude these for now ---
# all_data[rowSums(is.na(all_data)) > 0,]
all_data %>%
  filter(!is.na(rf)) -> all_data


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

write.csv(bias_table, file = "code/VARtable.csv")
