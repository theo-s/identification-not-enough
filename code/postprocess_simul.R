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

# Now get Bias -----------------------------------------------------------------
all_data %>%
  dplyr::mutate(across(-c(1,2), ~  . - TrueATE)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,Exp) %>%
  summarise(across(everything(), list(mean))) -> bias_table


write.csv(bias_table, file = "code/results/BIAStable.csv")

# Now get MSE ------------------------------------------------------------------

all_data %>%
  dplyr::mutate(across(-c(1,2), ~  . - TrueATE)) %>%
  dplyr::mutate(across(-c(1,2), ~  . **2)) %>%
  dplyr::select(-TrueATE) %>%
  group_by(N,Exp) %>%
  summarise(across(everything(), list(mean))) -> mse_table

write.csv(bias_table, file = "code/results/MSEtable.csv")


# Get variance -----------------------------------------------------------------
bias_squared <- bias_table[,-c(1,2)]**2
bias_squared[is.na(bias_squared)] <- 0

mse_table[,-c(1,2)] - bias_squared -> var_table
var_table <- cbind(mse_table[,c(1,2)],var_table)

write.csv(bias_table, file = "code/results/VARtable.csv")
