rm(list = ls()) # clear workspace, use if needed

library(dplyr)

data <- read.csv("../datasets/energy_data/energydata_complete.csv")
data <- subset(data, select = c("T1", "T2", "T3","T4","T5", "Appliances","lights", "T_out","Press_mm_hg", "Windspeed"))
# anomalies
data$anomaly <- FALSE

# https://archive.ics.uci.edu/ml/datasets/Appliances+energy+prediction#
# Attribute Information:
#   
# date time year-month-day hour:minute:second
# Appliances, energy use in Wh
# lights, energy use of light fixtures in the house in Wh
# T1, Temperature in kitchen area, in Celsius
# RH_1, Humidity in kitchen area, in %
# T2, Temperature in living room area, in Celsius
# RH_2, Humidity in living room area, in %
# T3, Temperature in laundry room area
# RH_3, Humidity in laundry room area, in %
# T4, Temperature in office room, in Celsius
# RH_4, Humidity in office room, in %
# T5, Temperature in bathroom, in Celsius
# RH_5, Humidity in bathroom, in %
# T6, Temperature outside the building (north side), in Celsius
# RH_6, Humidity outside the building (north side), in %
# T7, Temperature in ironing room , in Celsius
# RH_7, Humidity in ironing room, in %
# T8, Temperature in teenager room 2, in Celsius
# RH_8, Humidity in teenager room 2, in %
# T9, Temperature in parents room, in Celsius
# RH_9, Humidity in parents room, in %
# To, Temperature outside (from Chievres weather station), in Celsius
# Pressure (from Chievres weather station), in mm Hg
# RH_out, Humidity outside (from Chievres weather station), in %
# Wind speed (from Chievres weather station), in m/s
# Visibility (from Chievres weather station), in km
# Tdewpoint (from Chievres weather station), Â°C
# rv1, Random variable 1, nondimensional
# rv2, Random variable 2, nondimensional

x <- 1:576

# plot 1
par(mfrow=c(3,1))
plot(x,data[1:576,]$T1, type="l", col="blue")
lines(x,data[1:576,]$T2,type="l",col="green")
lines(x,data[1:576,]$T3,type="l",col="yellow")

plot(x,data[1:576,]$Appliances, type="l", col="red")



plot(x,data[1:576,]$T_out, type="l", col="red")

# plot 2
x <- 2592:3168

par(mfrow=c(3,1))
plot(x,data[2592:3168,]$T1, type="l", col="red")
lines(x,data[2592:3168,]$T2,type="l",col="green")
lines(x,data[2592:3168,]$T3,type="l",col="green")

plot(x,data[2592:3168,]$Appliances, type="l", col="red")

plot(x,data[2592:3168,]$T_out, type="l", col="red")


# plot 3
x <- 2592:5184
par(mfrow=c(4,1))
plot(x,data[2592:5184,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps")
lines(x,data[2592:5184,]$T2,type="l",col="green")
lines(x,data[2592:5184,]$T3,type="l",col="red")

plot(x,data[2592:5184,]$Appliances, type="l", col="red", ylab = "Energy Use (W/h)", xlab = "Timesteps")

plot(x,data[2592:5184,]$T_out, type="l", col="red", ylab = "Outside Temp. (C°) ", xlab = "Timesteps")

plot(x, data[2592:5184,]$Windspeed, type = "l", col="red", ylab = "Wind Speed (m/s)", xlab = "Timesteps")


#Plot 4
x <- 16000:19000
par(mfrow=c(4,1))
plot(x,data[16000:19000,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(20,27))
lines(x,data[16000:19000,]$T2,type="l",col="green")
lines(x,data[16000:19000,]$T3,type="l",col="red")

plot(x,data[16000:19000,]$Appliances, type="l", col="red", ylab = "Energy Use (W/h)", xlab = "Timesteps")

plot(x,data[16000:19000,]$T_out, type="l", col="red", ylab = "Outside Temp. (C°) ", xlab = "Timesteps")

plot(x, data[16000:19000,]$Windspeed, type = "l", col="red", ylab = "Wind Speed (m/s)", xlab = "Timesteps")









# 20000 datapoints. 18000 for training -> 270 anomalous datapoints
# anomalies are marked on the full hour 
# anomalies are of different length

#1 Level Shift one Room Temp is too high/low
# two each

#1
par(mfrow=c(4,1))
x <- 14581:15157
plot(x,data[14581:15157,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,27))
lines(x,data[14581:15157,]$T2,type="l",col="green")
lines(x,data[14581:15157,]$T3,type="l",col="red")


for (i in 1:75) {
  data[14620+i,]$T1 <- data[14620+i,]$T1 + 0.035 *i
}
for (i in 1:74) {
  data[14770-i,]$T1 <- data[14770-i,]$T1 + 0.035 *i
}
data[14620:14770,]$anomaly <- TRUE


x <- 14581:15157
plot(x,data[14581:15157,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,27))
lines(x,data[14581:15157,]$T2,type="l",col="green")
lines(x,data[14581:15157,]$T3,type="l",col="red")


#2
par(mfrow=c(2,1))
x <- 9301:9877
plot(x,data[9301:9877,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,27))
lines(x,data[9301:9877,]$T2,type="l",col="green")
lines(x,data[9301:9877,]$T3,type="l",col="red")

for (i in 1:40) {
  data[9640+i,]$T1 <- data[9640+i,]$T1 + 0.035 *i
}
for (i in 1:39) {
  data[9720-i,]$T1 <- data[9720-i,]$T1 + 0.035 *i
}
data[9640:9720,]$anomaly <- TRUE

x <- 9301:9877
plot(x,data[9301:9877,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,27))
lines(x,data[9301:9877,]$T2,type="l",col="green")
lines(x,data[9301:9877,]$T3,type="l",col="red")

#3
par(mfrow=c(2,1))
x <- 17511:18000
plot(x,data[17511:18000,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(20,29))
lines(x,data[17511:18000,]$T2,type="l",col="green")
lines(x,data[17511:18000,]$T3,type="l",col="red")

for (i in 1:130) {
  data[17650+i,]$T2 <- jitter(22.8 - i * 0.007, factor = 0.1)
}
data[17650:17780,]$anomaly <- TRUE


x <- 17511:18000
plot(x,data[17511:18000,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(20,29))
lines(x,data[17511:18000,]$T2,type="l",col="green")
lines(x,data[17511:18000,]$T3,type="l",col="red")

#4
par(mfrow=c(4,1))
x <- 8500:9076
plot(x,data[8500:9076,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,24))
lines(x,data[8500:9076,]$T2,type="l",col="green")
lines(x,data[8500:9076,]$T3,type="l",col="red")

data[8730:8890,]$T2 <- (data[8730:8890,]$T2 - 16) / 5 +16 
data[8730:8890,]$anomaly <- TRUE

x <-  8500:9076
plot(x,data[ 8500:9076,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,24))
lines(x,data[ 8500:9076,]$T2,type="l",col="green")
lines(x,data[ 8500:9076,]$T3,type="l",col="red")

#2 deviant cycle room temperatures dont rise as expected, all 3 are affected
# two

#5
par(mfrow=c(1,1))
x <- 4691:5267
plot(x,data[4691:5267,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,24))
lines(x,data[4691:5267,]$T2,type="l",col="green")
lines(x,data[4691:5267,]$T3,type="l",col="red")


x <- 1:140
sin(x)
plot(x,sin(x/50), type="l", col="blue")

data[5001:5140,]$T1 <- ((data[5001:5140,]$T1 -20) / 5 ) + 19.6
data[5020:5130,]$T3 <- data[5020:5130,]$T3 -1
data[5001:5050,]$T3 <- data[5051:5100,]$T3

data[5001:5140,]$T2 <- ((data[5001:5140,]$T2 -18) / 5) + 18
data[5001:5140,]$anomaly <- TRUE

x <- 4691:5267
plot(x,data[4691:5267,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,24))
lines(x,data[4691:5267,]$T2,type="l",col="green")
lines(x,data[4691:5267,]$T3,type="l",col="red")

#6
par(mfrow=c(2,1))
x <- 7767:8343
plot(x,data[7767:8343,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,24))
lines(x,data[7767:8343,]$T2,type="l",col="green")
lines(x,data[7767:8343,]$T3,type="l",col="red")

data[8000:8160,]$T2 <- ((data[8000:8160,]$T2 -17) / 5 ) + 16.8
data[8000:8160,]$T1 <- ((data[8000:8160,]$T1 -19) / 5 ) + 19.2
data[8000:8180,]$T3 <- ((data[8000:8180,]$T3 -19.5) / 8 ) + 19.5
data[8000:8180,]$anomaly <- TRUE

x <- 7767:8343
plot(x,data[7767:8343,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,24))
lines(x,data[7767:8343,]$T2,type="l",col="green")
lines(x,data[7767:8343,]$T3,type="l",col="red")


#6.1
par(mfrow=c(2,1))
x <- 13000:13576
plot(x,data[13000:13576,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,26))
lines(x,data[13000:13576,]$T2,type="l",col="green")
lines(x,data[13000:13576,]$T3,type="l",col="red")

data[13200:13340,]$T2 <- (data[13200:13340,]$T2 -19.8) / 5 +19.5
data[13200:13350,]$T3 <- (data[13200:13350,]$T3 -23) / 5 +22.8
data[13200:13300,]$T1 <- (data[13200:13300,]$T1 - 22) / 5 + 21.8
data[13200:13350,]$anomaly <- TRUE

plot(x,data[13000:13576,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,26))
lines(x,data[13000:13576,]$T2,type="l",col="green")
lines(x,data[13000:13576,]$T3,type="l",col="red")


#4 distribution based aggregate anomaly, difference between T2 and T3
# 3 difference between T1 and T3 is too high


# difference between T1 and T3
diff3 <- as.data.frame(data$T3 - data$T1)

#7
par(mfrow=c(2,1))
x <- 502:1078
plot(x,data[502:1078,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(16,25))
lines(x,data[502:1078,]$T3,type="l",col="red")

data[520:650,]$T3 <- ((data[520:650,]$T3 -20) / 2 ) + 19
data[650:653,]$T3 <- ((data[650:653,]$T3 -20) / 2 ) + 19.3
data[653:656,]$T3 <- ((data[650:653,]$T3 -20) / 2 ) + 19.6
data[656:659,]$T3 <- ((data[656:659,]$T3 -20) / 2 ) + 20

data[520:650,]$T1 <- ((data[520:650,]$T1 -21) * 1.5 ) + 21
data[520:650,]$anomaly <- TRUE

x <- 502:1078
plot(x,data[502:1078,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(19,27))
lines(x,data[502:1078,]$T3,type="l",col="red")

#8
x <- 12000:12576
plot(x,data[12000:12576,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(19,27))
lines(x,data[12000:12576,]$T3,type="l",col="red")

data[12050:12160,]$T1 <- ((data[12050:12160,]$T1 -22) / 2 ) + 21.8
data[12050:12160,]$T3 <- ((data[12050:12160,]$T3 -24) * 1.5 ) + 24
data[12050:12160,]$anomaly <- TRUE

plot(x,data[12000:12576,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(19,27))
lines(x,data[12000:12576,]$T3,type="l",col="red")


#9
par(mfrow=c(1,1))
x <- 3456:4032
plot(x,data[3456:4032,]$Appliances, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps")

data[3804:3816,]$Appliances <- 20
data[3804:3816,]$anomaly <- TRUE

plot(x,data[3456:4032,]$Appliances, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps")


#10
par(mfrow=c(1,1))
x <- 5658:6234
plot(x,data[5658:6234,]$Appliances, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps")

data[5832:5844,]$Appliances <- 20
data[5832:5844,]$anomaly <- TRUE

plot(x,data[5658:6234,]$Appliances, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps")

#11
par(mfrow=c(1,1))
x <- 16000:16576
plot(x,data[16000:16576,]$Appliances, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps")

data[16242:16254,]$Appliances <- 30
data[16242:16254,]$anomaly <- TRUE

plot(x,data[16000:16576,]$Appliances, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps")

#12
par(mfrow=c(1,1))
x <- 11000:11576
plot(x,data[11000:11576,]$Appliances, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps")

data[11154:11166,]$Appliances <- 30
data[11154:11166,]$anomaly <- TRUE

plot(x,data[11000:11576,]$Appliances, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps")

summary(data)

# 5% are anomalous in training


# Validation Set

#1
par(mfrow=c(2,1))
x <- 18000:18576
plot(x,data[18000:18576,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(19,27))
lines(x,data[18000:18576,]$T2,type="l",col="green")
lines(x,data[18000:18576,]$T3,type="l",col="red")

data[18400:18550,]$T3 <- data[18220:18370,]$T2 +3.4
data[18400:18550,]$anomaly <- TRUE

plot(x,data[18000:18576,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(19,30))
lines(x,data[18000:18576,]$T2,type="l",col="green")
lines(x,data[18000:18576,]$T3,type="l",col="red")

#2
x <- 19000:19576
plot(x,data[19000:19576,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(19,27))
lines(x,data[19000:19576,]$T2,type="l",col="green")
lines(x,data[19000:19576,]$T3,type="l",col="red")

data[19240:19385,]$T2 <- (data[19240:19385,]$T2 - 21.7) / 5 + 21.5
data[19240:19385,]$T1 <- (data[19240:19385,]$T1 - 24) / 5 + 23.7
data[19240:19385,]$anomaly <- TRUE

plot(x,data[19000:19576,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(19,27))
lines(x,data[19000:19576,]$T2,type="l",col="green")
lines(x,data[19000:19576,]$T3,type="l",col="red")

#3
x <- 19000:19576
plot(x,data[19000:19576,]$Appliances, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps")

data[19350:19362,]$Appliances <- 20
data[19350:19362,]$anomaly <- TRUE

plot(x,data[19000:19576,]$Appliances, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps")


#4
x <- 19576:19735
plot(x,data[19576:19735,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(19,27))
lines(x,data[19576:19735,]$T3,type="l",col="red")


data[19580:19620,]$T1 <- (data[19580:19620,]$T3 - 26) * -1.5 + 24.5
data[19580:19620,]$anomaly <- TRUE

plot(x,data[19576:19735,]$T1, type="l", col="blue", ylab = "Room Temp. (C°)", xlab = "Timesteps", ylim = c(19,27))
lines(x,data[19576:19735,]$T3,type="l",col="red")

save(data, file = "../datasets/energy_data/energy_data_with_anomalies")



