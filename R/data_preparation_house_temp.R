library(dplyr)

rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/energy_data/energydata_complete.csv")
data <- subset(data, select = c("T1", "T2", "T3","T4","T5", "Appliances","lights", "T_out","Press_mm_hg", "Windspeed"))
# anomalies
data$anomaly <- FALSE

data <- data[1:19728,] # keep full hours

data$Appliances <- smooth(data$Appliances)

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

data_train <- data_train[!is.na(data_train$T1),]


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

data_test[1245:1265,]$T2 <- (data_test[1245:1265,]$T2 - 18.5) * 2.5 + 18.5
data_test[1245:1265,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(16,23), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")


index <- runif(1, min = 1, max = nrow(data_test))
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

data_test[1980:2010,]$T2 <- (data_test[1980:2010,]$T2 - 19.5) / 5 + 19.5
data_test[1980:2010,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

index <- runif(1, min = 1, max = nrow(data_test))
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(20,27), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

data_test[3040:3060,]$T1 <- (data_test[3040:3060,]$T1 - 23) * 2.5 + 23
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

data_test[670:690,]$T1 <- (data_test[670:690,]$T1 - 21) / 5 + 21
data_test[670:690,]$T2 <- (data_test[670:690,]$T2 - 20) / 5 + 20
data_test[670:690,]$T3 <- (data_test[670:690,]$T3 - 22) / 5 + 22
data_test[670:690,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

index <- runif(1, min = 1, max = nrow(data_test))
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(18,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")
lines(x, data_test[x,]$T3, type = "l", col = "red")

data_test[2340:2365,]$T1 <- (data_test[2340:2365,]$T1 - 21) / 4 + 21
data_test[2340:2365,]$T2 <- (data_test[2340:2365,]$T2 - 18) / 5 + 18
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

data_test[2010:2030,]$T1 <- (data_test[2010:2030,]$T1 - 22) / 4 + 22
data_test[2010:2030,]$T2 <- (data_test[2010:2030,]$T2 - 20) / 5 + 20
data_test[2010:2030,]$T3 <- (data_test[2010:2030,]$T3 - 23) / 4 + 23
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
par(mfrow = c(1,1))

diff <- as.data.frame( data_test$T1 - data_test$T2)
index <- 1450
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(16,23), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")

data_test[1505:1526,]$T1 <- (data_test[1505:1526,]$T1 - 20.2) * 1.5 + 20.2
data_test[1505:1526,]$T2 <- (data_test[1505:1526,]$T2 - 16.5) / 3 + 16.5
data_test[1505:1526,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(16,23), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")

index <- 2270
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(16,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")

data_test[2320:2340,]$T1 <- (data_test[2320:2340,]$T1 - 21.5) * 1.5 + 21.5
data_test[2325:2350,]$T2 <- (data_test[2325:2350,]$T2 - 22) * 1.5 + 22
data_test[2320:2350,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(16,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")

index <- 2754
x <- index:(index + 288)
plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(20,29), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")

data_test[2825:2848,]$T1 <- (data_test[2825:2848,]$T1 - 24) * 0.8 + 24
data_test[2825:2848,]$T2 <- (data_test[2825:2848,]$T2 - 22.5) * 1.2 + 22.5
data_test[2825:2848,]$anomaly <- TRUE

plot(x, data_test[x,]$T1, type = "l", col = "green", ylim = c(20,29), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_test[x,]$T2, type = "l", col = "blue")

save(data_test, file = "../datasets/energy_data/energy_data_test_with_anomalies_3")

###############################
#                             #
# Training Data Preparation   #

rm(list = ls()) # clear workspace, use if needed

load(file = "../datasets/energy_data/energy_data_train")

index <- runif(1, min = 1, max = nrow(data_train))

par(mfrow = c(1,1))
index <- 2728
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(20,28), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

data_train[2800:2825,]$T2 <- (data_train[2800:2825,]$T2 - 23) / 4 + 23
data_train[2800:2825,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(20,28), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

index <- 9936
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

data_train[10080:10100,]$T3 <- (data_train[10080:10100,]$T3 - 18) * 3 + 18
data_train[10080:10100,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

index <- 4843
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

data_train[5055:5075,]$T1 <- (data_train[5055:5075,]$T1 - 22) /3.5 + 22
data_train[5055:5075,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

index <- 12600
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(19,28), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

data_train[12730:12755,]$T2 <- (data_train[12730:12755,]$T2 - 23) /3.5 + 23
data_train[12730:12755,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(19,28), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

index <- 14200
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

data_train[14275:14290,]$T2 <- (data_train[14275:14290,]$T2 - 18) * 2 + 18
data_train[14275:14290,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")


index <- 6800
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

data_train[6815:6835,]$T1 <- (data_train[6815:6835,]$T1 - 18) / 4 + 18
data_train[6815:6835,]$T2 <- (data_train[6815:6835,]$T2 - 17) / 4 + 17
data_train[6815:6835,]$T3 <- (data_train[6815:6835,]$T3 - 18) / 3 + 18
data_train[6815:6835,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

index <- 15000
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(18,26), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

data_train[15160:15180,]$T1 <- (data_train[15160:15180,]$T1 - 22) / 4 + 22
data_train[15160:15180,]$T2 <- (data_train[15160:15180,]$T2 - 20) / 4 + 20
data_train[15160:15180,]$T3 <- (data_train[15160:15180,]$T3 - 23) / 3 + 23
data_train[15160:15180,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(18,26), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

index <- 1000
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(17,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

data_train[1170:1190,]$T1 <- (data_train[1170:1190,]$T1 - 20) / 4 + 20
data_train[1170:1190,]$T2 <- (data_train[1170:1190,]$T2 - 17.5) / 4 + 17.5
data_train[1170:1190,]$T3 <- (data_train[1170:1190,]$T3 - 20) / 4 + 20
data_train[1170:1190,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(17,25), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")
lines(x, data_train[x,]$T3, type = "l", col = "red")

index <- 1500
x <- index:(index + 288)
plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

data_train[1590:1610,]$Appliances <- 60
data_train[1590:1610,]$anomaly <- TRUE

plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

index <- 5000
x <- index:(index + 288)
plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

data_train[5230:5250,]$Appliances <- 30
data_train[5230:5250,]$anomaly <- TRUE

plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

index <- 11800
x <- index:(index + 288)
plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

data_train[11830:11850,]$Appliances <- 30
data_train[11830:11850,]$anomaly <- TRUE

plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

index <- 8500
x <- index:(index + 288)
plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

data_train[8535:8550,]$Appliances <- 40
data_train[8535:8550,]$anomaly <- TRUE

plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

index <- 12000
x <- index:(index + 288)
plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

data_train[12100:12115,]$Appliances <- 40
data_train[12100:12115,]$anomaly <- TRUE

plot(x, data_train[x,]$Appliances, type = "l", col = "red", ylab = "Energy in Use (Wh)", xlab = "Timesteps (h)")

diff <- as.data.frame( data_train$T1 - data_train$T2)

index <- 4744
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")

data_train[4800:4825,]$T1 <- (data_train[4800:4825,]$T1 - 20) * 1.2 + 20
data_train[4800:4825,]$T2 <- (data_train[4800:4825,]$T2 - 20) * 1.2 + 20
data_train[4800:4825,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")

index <- 8031
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")

data_train[8070:8105,]$T1 <- (data_train[8070:8105,]$T1 - 20) * 1.2 + 20
data_train[8085:8105,]$T2 <- (data_train[8085:8105,]$T2 - 20) * 1.8 + 20
data_train[8070:8105,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(16,24), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")

index <- 9329
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(20,28), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")

data_train[9395:9420,]$T2 <- (data_train[9395:9420,]$T2 - 22) * 1.5 + 22
data_train[9395:9420,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(20,29), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")

index <- 15905
x <- index:(index + 288)
plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(20,28), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")

data_train[15975:15995,]$T1 <- (data_train[15975:15995,]$T1 - 24) / 1.5 + 24
data_train[15975:15995,]$T2 <- (data_train[15975:15995,]$T2 - 22) * 1.2 + 22
data_train[15975:15995,]$anomaly <- TRUE

plot(x, data_train[x,]$T1, type = "l", col = "green", ylim = c(20,29), ylab = "Room Temp (C°)", xlab = "Timesteps (h)")
lines(x, data_train[x,]$T2, type = "l", col = "blue")

save(data_train, file = "../datasets/energy_data/energy_data_train_with_anomalies")
