library(ggplot2)
library(dplyr)
library(viridis)

all_data <- data.frame()

for (file in dir("code/results")) {
  print(file)
  data <- readRDS(paste0("code/results/",file))
  all_data <- rbind(all_data, data)
}

all_data$TrueATE <- ifelse(all_data$Exp==2,.2058,.25)

# Now get Bias and MSE
