rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../../datasets/GHL/train_1500000_seed_11_vars_23.csv")

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


save(data, file = "../../datasets/GHL/GHL_training_data")


# investigation of a test dataset
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23.csv")

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

save(data, file = "../../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23")

# investigation of a test dataset
rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23.csv")

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

save(data, file = "../../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23.1")

################################################################################

#preparation of test sets

rm(list = ls())

data <- NULL

setwd("~/MSCBIS/MT/trunk/R")

files <-list.files("../../datasets/GHL/", pattern=".csv", all.files=FALSE,
           full.names=FALSE)

setwd("../../datasets/GHL/")

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

save(data, file = "../../datasets/GHL/GHL_test_sample_5")

rm(list = ls())

data <- NULL

setwd("~/MSCBIS/MT/trunk/R")

files <-list.files("../../datasets/GHL/", pattern=".csv", all.files=FALSE,
                   full.names=FALSE)

setwd("../../datasets/GHL/")

for (i in 1:48) {
  filename <- files[i]
  temp <- read.csv(filename)
  temp <- temp[1:204300,]
  temp <- temp[c(-1)]
  temp <- temp %>%
    filter(row_number() %% 10 == 1)
  
  data <- rbind(data, temp)
}

setwd("~/MSCBIS/MT/trunk/R")

save(data, file = "../../datasets/GHL/GHL_control_sample_5")

rm(list = ls())

data <- read.csv("../../datasets/GHL/train_1500000_seed_11_vars_23.csv")

data <- data %>%
  filter(row_number() %% 10 == 1)

data <- subset(data, select = 2:20)

save(data, file = "../../datasets/GHL/GHL_training_data_sample5")
