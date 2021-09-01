library(dplyr)

rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/energy_data/energydata_complete.csv")
data <- subset(data, select = c("T1", "T2", "T3","T4","T5", "Appliances","lights", "T_out","Press_mm_hg", "Windspeed"))
# anomalies
data$anomaly <- FALSE

data <- data[1:19728,] # keep full hours

data_sampled_1 <- NULL
data_sampled_2 <- NULL
data_sampled_3 <- NULL
data_sampled_4 <- NULL
data_sampled_5 <- NULL

for (i in 1:3288) {
  k <- i * 6
  sample <- data[sample((k-5):k, 1, replace = FALSE), ]
  data_sampled_1 <- rbind(data_sampled_1,sample)
}

data <- rbind(data, data_sampled_1)
data_cp <- data[!(duplicated(data) | duplicated(data, fromLast = TRUE)), ]

for (i in 1:3288) {
  k <- i * 5
  sample <- data_cp[sample((k-4):k, 1, replace = FALSE), ]
  data_sampled_2 <- rbind(data_sampled_2,sample)
}

data <- rbind(data, data_sampled_2)
data_cp <- data[!(duplicated(data) | duplicated(data, fromLast = TRUE)), ]

for (i in 1:3288) {
  k <- i * 4
  sample <- data_cp[sample((k-3):k, 1, replace = FALSE), ]
  data_sampled_3 <- rbind(data_sampled_3,sample)
}

data <- rbind(data, data_sampled_3)
data_cp <- data[!(duplicated(data) | duplicated(data, fromLast = TRUE)), ]

for (i in 1:3288) {
  k <- i * 3
  sample <- data_cp[sample((k-2):k, 1, replace = FALSE), ]
  data_sampled_4 <- rbind(data_sampled_4,sample)
}

data <- rbind(data, data_sampled_4)
data_cp <- data[!(duplicated(data) | duplicated(data, fromLast = TRUE)), ]

for (i in 1:3288) {
  k <- i * 2
  sample <- data_cp[sample((k-1):k, 1, replace = FALSE), ]
  data_sampled_5 <- rbind(data_sampled_5,sample)
}

data <- rbind(data, data_sampled_5)
data_test <- data[!(duplicated(data) | duplicated(data, fromLast = TRUE)), ]


data_train <- rbind(data_sampled_1, data_sampled_2, data_sampled_3, data_sampled_4, data_sampled_5)

save(data_train, file = "../datasets/energy_data/energy_data_train" )
save(data_test, file = "../datasets/energy_data/energy_data_test")

rm(list = ls()) # clear workspace, use if needed

load(file = "../datasets/energy_data/energy_data_train")

par(mfrow = c(3,1))
x <- 1:288 
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(17,23), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

plot(x, data_train[x,]$T_out, type = "l", col = "red", ylab = "Outside Temp (C°)", xlab = "Timesteps (h)")


x <- 1000:1288 
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(17,23), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

plot(x, data_train[x,]$T_out, type = "l", col = "red", ylab = "Outside Temp (C°)", xlab = "Timesteps (h)")

rm(list = ls()) # clear workspace, use if needed

load(file = "../datasets/energy_data/energy_data_test")

set.seed(1)
index <- runif(1, min = 1, max = nrow(data_test))

par(mfrow = c(1,1))
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(17,23), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

data_test[925:955,]$T1 <- (data_test[925:955,]$T1 - 19.5) / 2 + 19.5
data_test[925:955,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(17,23), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

index <- runif(1, min = 1, max = nrow(data_test))

par(mfrow = c(1,1))
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(16,23), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

data_test[1245:1265,]$T2 <- (data_test[1245:1265,]$T2 - 18.5) * 1.5 + 18.5
data_test[1245:1265,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(16,23), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")


index <- runif(1, min = 1, max = nrow(data_test))
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

data_test[1980:2010,]$T2 <- (data_test[1980:2010,]$T2 - 19.5) / 2 + 19.5
data_test[1980:2010,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

index <- runif(1, min = 1, max = nrow(data_test))
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(20,27), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

data_test[3040:3060,]$T1 <- (data_test[3040:3060,]$T1 - 23) * 2.2 + 23
data_test[3040:3060,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(20,27), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

save(data_test, file = "../datasets/energy_data/energy_data_test_with_anomalies_1")

rm(list = ls()) # clear workspace, use if needed

load(file = "../datasets/energy_data/energy_data_test")

set.seed(2)
index <- runif(1, min = 1, max = nrow(data_test))
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

data_test[670:690,]$T1 <- (data_test[670:690,]$T1 - 21) / 2.8 + 21
data_test[670:690,]$T2 <- (data_test[670:690,]$T2 - 20) / 2.8 + 20
data_test[670:690,]$T3 <- (data_test[670:690,]$T3 - 22) / 2.8 + 22
data_test[670:690,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

index <- runif(1, min = 1, max = nrow(data_test))
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

data_test[2340:2365,]$T1 <- (data_test[2340:2365,]$T1 - 21) / 2 + 21
data_test[2340:2365,]$T2 <- (data_test[2340:2365,]$T2 - 18) / 2.8 + 18
data_test[2340:2365,]$T3 <- (data_test[2340:2365,]$T3 - 22.1) / 2.8 + 22.1
data_test[2340:2365,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

index <- runif(1, min = 1, max = nrow(data_test))
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

data_test[2010:2030,]$T1 <- (data_test[2010:2030,]$T1 - 22) / 2.8 + 22
data_test[2010:2030,]$T2 <- (data_test[2010:2030,]$T2 - 20) / 2.8 + 20
data_test[2010:2030,]$T3 <- (data_test[2010:2030,]$T3 - 23) / 2.8 + 23
data_test[2010:2030,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

index <- runif(1, min = 1, max = nrow(data_test))
x <- index:(index + 288)
plot(x, data_test[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

data_test[585:598,]$Appliances <- 50
data_test[585:598,]$anomaly <- TRUE

plot(x, data_test[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

index <- runif(1, min = 1, max = nrow(data_test))
x <- index:(index + 288)
plot(x, data_test[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

data_test[3110:3125,]$Appliances <- 60
data_test[3110:3125,]$anomaly <- TRUE

plot(x, data_test[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

index <- runif(1, min = 1, max = nrow(data_test))
index <- runif(1, min = 1, max = nrow(data_test))
x <- index:(index + 288)
plot(x, data_test[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

data_test[430:445,]$Appliances <- 40
data_test[430:445,]$anomaly <- TRUE

plot(x, data_test[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

save(data_test, file = "../datasets/energy_data/energy_data_test_with_anomalies_2")

rm(list = ls()) # clear workspace, use if needed

load(file = "../datasets/energy_data/energy_data_test")

set.seed(3)
