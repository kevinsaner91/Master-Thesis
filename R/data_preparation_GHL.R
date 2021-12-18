rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/train_1500000_seed_11_vars_23.csv")

#data <- subset(data, select = c("RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))

data <- data %>%
  filter(row_number() %% 10 == 1)

data <- subset(data, select = 2:20)


par(mfrow = c(5,1))
x <- 1:5000 
plot(x, data[x,]$RT_level, type = "l", col = "green",  ylab = "RT Level", xlab = "Timesteps")

plot(x, data[x,]$RT_temperature.T, type = "l", col = "green",  ylab = "RT Temperature", xlab = "Timesteps")

plot(x, data[x,]$HT_temperature.T, type = "l", col = "green",  ylab = "HT Temperature", xlab = "Timesteps")

plot(x, data[x,]$inj_valve_act, type = "l", col = "green",  ylab = "Inj Valve Act", xlab = "Timesteps")

plot(x, data[x,]$heater_act, type = "l", col = "green",  ylab = "Heater Act", xlab = "Timesteps")


save(data, file = "../datasets/GHL/GHL_training_data")


# investigation of a test dataset
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

par(mfrow = c(5,1))
x <- 1:5000 
plot(x, data[x,]$RT_level, type = "l", col = "green",  ylab = "RT Level", xlab = "Timesteps")

plot(x, data[x,]$RT_temperature.T, type = "l", col = "green",  ylab = "RT Temperature", xlab = "Timesteps")

plot(x, data[x,]$HT_temperature.T, type = "l", col = "green",  ylab = "HT Temperature", xlab = "Timesteps")

plot(x, data[x,]$inj_valve_act, type = "l", col = "green",  ylab = "Inj Valve Act", xlab = "Timesteps")

plot(x, data[x,]$heater_act, type = "l", col = "green",  ylab = "Heater Act", xlab = "Timesteps")

#plot(x, data[x,]$DANGER, type = "l", col = "green",  ylab = "Danger", xlab = "Timesteps")

save(data, file = "../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23")

# investigation of a test dataset
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-4,-21)]

par(mfrow = c(5,1))
x <- 1:5000 
plot(x, data[x,]$RT_level, type = "l", col = "green",  ylab = "RT Level", xlab = "Timesteps")

plot(x, data[x,]$RT_temperature.T, type = "l", col = "green",  ylab = "RT Temperature", xlab = "Timesteps")

plot(x, data[x,]$HT_temperature.T, type = "l", col = "green",  ylab = "HT Temperature", xlab = "Timesteps")

plot(x, data[x,]$inj_valve_act, type = "l", col = "green",  ylab = "Inj Valve Act", xlab = "Timesteps")

plot(x, data[x,]$heater_act, type = "l", col = "green",  ylab = "Heater Act", xlab = "Timesteps")

#plot(x, data[x,]$DANGER, type = "l", col = "green",  ylab = "Danger", xlab = "Timesteps")

save(data, file = "../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/02_Lev_fault_Temp_corr_seed_17_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/02_Lev_fault_Temp_corr_seed_17_vars_23")

data <- read.csv("../datasets/GHL/02_Lev_fault_Temp_corr_seed_17_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-4,-21)]

save(data, file = "../datasets/GHL/02_Lev_fault_Temp_corr_seed_17_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/03_Lev_fault_Temp_corr_seed_19_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/03_Lev_fault_Temp_corr_seed_19_vars_23")

data <- read.csv("../datasets/GHL/03_Lev_fault_Temp_corr_seed_19_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-4,-21)]

save(data, file = "../datasets/GHL/03_Lev_fault_Temp_corr_seed_19_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/04_Lev_fault_Temp_corr_seed_23_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/04_Lev_fault_Temp_corr_seed_23_vars_23")

data <- read.csv("../datasets/GHL/04_Lev_fault_Temp_corr_seed_23_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-4,-21)]

save(data, file = "../datasets/GHL/04_Lev_fault_Temp_corr_seed_23_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/05_Lev_fault_Temp_corr_seed_27_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/05_Lev_fault_Temp_corr_seed_27_vars_23")

data <- read.csv("../datasets/GHL/05_Lev_fault_Temp_corr_seed_27_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-4,-21)]

save(data, file = "../datasets/GHL/05_Lev_fault_Temp_corr_seed_27_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/06_Lev_fault_Temp_corr_seed_29_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/06_Lev_fault_Temp_corr_seed_29_vars_23")

data <- read.csv("../datasets/GHL/06_Lev_fault_Temp_corr_seed_29_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-4,-21)]

save(data, file = "../datasets/GHL/06_Lev_fault_Temp_corr_seed_29_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/07_Lev_fault_Temp_corr_seed_31_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/07_Lev_fault_Temp_corr_seed_31_vars_23")

data <- read.csv("../datasets/GHL/07_Lev_fault_Temp_corr_seed_31_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/07_Lev_fault_Temp_corr_seed_31_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/08_Lev_fault_Temp_corr_seed_33_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/08_Lev_fault_Temp_corr_seed_33_vars_23")

data <- read.csv("../datasets/GHL/08_Lev_fault_Temp_corr_seed_33_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/08_Lev_fault_Temp_corr_seed_33_vars_23.1")


#
##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/09_Lev_fault_Temp_corr_seed_37_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/09_Lev_fault_Temp_corr_seed_37_vars_23")

data <- read.csv("../datasets/GHL/09_Lev_fault_Temp_corr_seed_37_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/09_Lev_fault_Temp_corr_seed_37_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/10_Lev_fault_Temp_corr_seed_39_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/10_Lev_fault_Temp_corr_seed_39_vars_23")

data <- read.csv("../datasets/GHL/10_Lev_fault_Temp_corr_seed_39_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/10_Lev_fault_Temp_corr_seed_39_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/11_Lev_fault_Temp_corr_seed_41_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/11_Lev_fault_Temp_corr_seed_41_vars_23")

data <- read.csv("../datasets/GHL/11_Lev_fault_Temp_corr_seed_41_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/11_Lev_fault_Temp_corr_seed_41_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/12_Lev_fault_Temp_corr_seed_43_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/12_Lev_fault_Temp_corr_seed_43_vars_23")

data <- read.csv("../datasets/GHL/12_Lev_fault_Temp_corr_seed_43_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/12_Lev_fault_Temp_corr_seed_43_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/13_Lev_fault_Temp_corr_seed_666_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/13_Lev_fault_Temp_corr_seed_666_vars_23")

data <- read.csv("../datasets/GHL/13_Lev_fault_Temp_corr_seed_666_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/13_Lev_fault_Temp_corr_seed_666_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/14_Lev_fault_Temp_corr_seed_47_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/14_Lev_fault_Temp_corr_seed_47_vars_23")

data <- read.csv("../datasets/GHL/14_Lev_fault_Temp_corr_seed_47_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/14_Lev_fault_Temp_corr_seed_47_vars_23.1")


##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/15_Lev_fault_Temp_corr_seed_49_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/15_Lev_fault_Temp_corr_seed_49_vars_23")

data <- read.csv("../datasets/GHL/15_Lev_fault_Temp_corr_seed_49_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/15_Lev_fault_Temp_corr_seed_49_vars_23.1")

##############################################################################################

# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/16_Lev_fault_Temp_corr_seed_53_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/16_Lev_fault_Temp_corr_seed_53_vars_23")

data <- read.csv("../datasets/GHL/16_Lev_fault_Temp_corr_seed_53_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/16_Lev_fault_Temp_corr_seed_53_vars_23.1")

##############################################################################################

#17_Lev_fault_Temp_corr_seed_57_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/17_Lev_fault_Temp_corr_seed_57_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/17_Lev_fault_Temp_corr_seed_57_vars_23")

data <- read.csv("../datasets/GHL/17_Lev_fault_Temp_corr_seed_57_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/17_Lev_fault_Temp_corr_seed_57_vars_23.1")

##############################################################################################

#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/18_Lev_fault_Temp_corr_seed_59_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/18_Lev_fault_Temp_corr_seed_59_vars_23")

data <- read.csv("../datasets/GHL/18_Lev_fault_Temp_corr_seed_59_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/18_Lev_fault_Temp_corr_seed_59_vars_23.1")

##############################################################################################

#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/19_Lev_fault_Temp_corr_seed_62_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/19_Lev_fault_Temp_corr_seed_62_vars_23")

data <- read.csv("../datasets/GHL/19_Lev_fault_Temp_corr_seed_62_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/19_Lev_fault_Temp_corr_seed_62_vars_23.1")

##############################################################################################

#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/20_Lev_fault_Temp_corr_seed_67_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/20_Lev_fault_Temp_corr_seed_67_vars_23")

data <- read.csv("../datasets/GHL/20_Lev_fault_Temp_corr_seed_67_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/20_Lev_fault_Temp_corr_seed_67_vars_23.1")

##############################################################################################

#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/21_Lev_fault_Temp_corr_seed_73_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/21_Lev_fault_Temp_corr_seed_73_vars_23")

data <- read.csv("../datasets/GHL/21_Lev_fault_Temp_corr_seed_73_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/21_Lev_fault_Temp_corr_seed_73_vars_23.1")

##############################################################################################

#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/22_Lev_fault_Temp_corr_seed_777_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/22_Lev_fault_Temp_corr_seed_777_vars_23")

data <- read.csv("../datasets/GHL/22_Lev_fault_Temp_corr_seed_777_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/22_Lev_fault_Temp_corr_seed_777_vars_23.1")

##############################################################################################

#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/23_Lev_fault_Temp_corr_seed_79_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/23_Lev_fault_Temp_corr_seed_79_vars_23")

data <- read.csv("../datasets/GHL/23_Lev_fault_Temp_corr_seed_79_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/23_Lev_fault_Temp_corr_seed_79_vars_23.1")

##############################################################################################

#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/24_Lev_fault_Temp_corr_seed_83_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/24_Lev_fault_Temp_corr_seed_83_vars_23")

data <- read.csv("../datasets/GHL/24_Lev_fault_Temp_corr_seed_83_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/24_Lev_fault_Temp_corr_seed_83_vars_23.1")

##############################################################################################
#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/25_Lev_corr_Temp_fault_seed_111_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/25_Lev_corr_Temp_fault_seed_111_vars_23")

data <- read.csv("../datasets/GHL/25_Lev_corr_Temp_fault_seed_111_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/25_Lev_corr_Temp_fault_seed_111_vars_23.1")

##############################################################################################
#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/26_Lev_corr_Temp_fault_seed_113_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/26_Lev_corr_Temp_fault_seed_113_vars_23")

data <- read.csv("../datasets/GHL/26_Lev_corr_Temp_fault_seed_113_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/26_Lev_corr_Temp_fault_seed_113_vars_23.1")

##############################################################################################
#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/27_Lev_corr_Temp_fault_seed_115_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/27_Lev_corr_Temp_fault_seed_115_vars_23")

data <- read.csv("../datasets/GHL/27_Lev_corr_Temp_fault_seed_115_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/27_Lev_corr_Temp_fault_seed_115_vars_23.1")

##############################################################################################
#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23")

data <- read.csv("../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23.1")

##############################################################################################
#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23")

data <- read.csv("../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23.1")


##############################################################################################
#18_Lev_fault_Temp_corr_seed_59_vars_23
# preparation of a test dataset 2
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1,-3,-4,-21)]

save(data, file = "../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23")

data <- read.csv("../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23.csv")

#data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))
data <- data %>%
  filter(row_number() %% 10 == 1)

data <- data[c(-1)]

save(data, file = "../datasets/GHL/28_Lev_corr_Temp_fault_seed_119_vars_23.1")


##############################################################################################

rm(list = ls())

data <- NULL

setwd("~/MSCBIS/MT/trunk/R")

files <-list.files("../datasets/GHL/", pattern=".csv", all.files=FALSE,
           full.names=FALSE)

setwd("../datasets/GHL/")

for (i in 1:48) {
  filename <- files[i]
  temp <- read.csv(filename)
  temp <- temp[1:204300,]
  temp <- temp[c(-1,-3,-4,-21)]
  temp <- temp %>%
    filter(row_number() %% 5 == 1)
  
  data <- rbind(data, temp)
}

setwd("~/MSCBIS/MT/trunk/R")

save(data, file = "../datasets/GHL/GHL_test_sample_5")

rm(list = ls())

data <- NULL

setwd("~/MSCBIS/MT/trunk/R")

files <-list.files("../datasets/GHL/", pattern=".csv", all.files=FALSE,
                   full.names=FALSE)

setwd("../datasets/GHL/")

for (i in 1:48) {
  filename <- files[i]
  temp <- read.csv(filename)
  temp <- temp[1:204300,]
  temp <- temp[c(-1)]
  temp <- temp %>%
    filter(row_number() %% 5 == 1)
  
  data <- rbind(data, temp)
}

setwd("~/MSCBIS/MT/trunk/R")

save(data, file = "../datasets/GHL/GHL_control_sample_5")

rm(list = ls())

data <- read.csv("../datasets/GHL/train_1500000_seed_11_vars_23.csv")

data <- data %>%
  filter(row_number() %% 5 == 1)

data <- subset(data, select = 2:20)

save(data, file = "../datasets/GHL/GHL_training_data_sample5")
