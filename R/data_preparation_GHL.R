rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/GHL/train_1500000_seed_11_vars_23.csv")

data <- subset(data, select = c("RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))

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

data <- subset(data, select = c("DANGER","RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))

par(mfrow = c(6,1))
x <- 46000:51000 
plot(x, data[x,]$RT_level, type = "l", col = "green",  ylab = "RT Level", xlab = "Timesteps")

plot(x, data[x,]$RT_temperature.T, type = "l", col = "green",  ylab = "RT Temperature", xlab = "Timesteps")

plot(x, data[x,]$HT_temperature.T, type = "l", col = "green",  ylab = "HT Temperature", xlab = "Timesteps")

plot(x, data[x,]$inj_valve_act, type = "l", col = "green",  ylab = "Inj Valve Act", xlab = "Timesteps")

plot(x, data[x,]$heater_act, type = "l", col = "green",  ylab = "Heater Act", xlab = "Timesteps")

plot(x, data[x,]$DANGER, type = "l", col = "green",  ylab = "Danger", xlab = "Timesteps")

save(data, file = "../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23")

