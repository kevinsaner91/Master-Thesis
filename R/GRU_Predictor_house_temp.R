library(ggplot2)
library(keras)
library(tidyverse)
library(tictoc)

rm(list = ls()) # clear workspace, use if needed

data <- read.csv("../datasets/energy_data/energydata_complete.csv")
data <- subset(data, select = c("T1", "T2", "T3", "Appliances", "T_out", "Windspeed"))

data <- data.matrix(data[,-7])

train_data <- data[1:45600,1:6]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data[,1:6] <- scale(data[,1:6], center = mean, scale = std)

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows), dim(data)[[-1]]-2))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[j,] <- data[rows[[j]] + delay,1:4]
    }           
    list(samples, targets)
  }
}

#Now here is the data generator you'll use. It yields a list `(samples, targets)`, where `samples` is one batch of input data and `targets` is the corresponding array of target temperatures. It takes the following arguments:

#* `data` -- The original array of floating-point data, which you normalized in listing 6.32.
#* `lookback` -- How many timesteps back the input data should go.
#* `delay` -- How many timesteps in the future the target should be.
#* `min_index` and `max_index` -- Indices in the `data` array that delimit which timesteps to draw from. This is useful for keeping a segment of the data for validation and another for testing.
#* `shuffle` -- Whether to shuffle the samples or draw them in chronological order.
#* `batch_size` -- The number of samples per batch.
#* `step` -- The period, in timesteps, at which you sample data. You'll set it 6 in order to draw one data point every hour.

lookback <- 120 # 5d in the past
step <- 1
delay <- 1 # 1d in the future
batch_size <- 128

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 10000,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 18000,
  max_index = 19735,
  step = step,
  batch_size = batch_size
)

val_steps <- (45600 - 43200 - lookback) / batch_size

model <- keras_model_sequential() %>% 
  layer_gru(units = 32,input_shape = list(NULL, ncol(data)), return_sequences = TRUE) %>%
  layer_gru(units = 32, return_sequences = TRUE) %>%
  layer_gru(units = 32) %>%
  layer_dense(units = 4)

model %>% compile(
  optimizer = optimizer_adam(),
  loss = "mae",
  metrics = "mae"
)

history <- model %>% fit(
  train_gen,
  steps_per_epoch = 200,
  epochs = 5,
  validation_data = val_gen,
  validation_steps = val_steps
)

summary(model)

loss <- model %>% evaluate(test_gen, steps = test_steps, verbose = TRUE)



save_model_hdf5 (model, "GRU_Predictor_house_temp", include_optimizer = TRUE)


####
##
## Use the model to make predictions
##
####

rm(list = ls()) # clear workspace, use if needed

model <- load_model_hdf5("GRU_Predictor_syn", compile = TRUE)

load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset")

data <- data.matrix(data[,-1])

train_data <- data[1:45600,1:5]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data[,1:5] <- scale(data[,1:5], center = mean, scale = std)

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
    }            
    
    list(samples)
  }
}

lookback <- 120 # 5d in the past
step <- 1
delay <- 1 # 1h in the future
batch_size <- 0

test_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 45600,
  max_index = 48000,
  step = step,
  batch_size = batch_size
)

test_steps <- (48000 - 45600 - lookback) / batch_size

result <- predict(model, test_gen, steps = 72, verbose = TRUE)

result <- as.data.frame(result)
colnames(result)[1] <- "y"
colnames(result)[2] <- "y2"
colnames(result)[3] <- "y3"
colnames(result)[4] <- "y4"
colnames(result)[5] <- "y5"

result$x <- 1:nrow(result)

data <- as.data.frame(data)
data$x <- 1:nrow(data)
plot(data[45721:45792,]$x,data[45721:45792,]$y,type = "l")
plot(data[1:72,]$x,data[1:72,]$y,type = "l" )


plot(result[1:72,]$x,result[1:72,]$y, type="l", col="red")
lines(result[1:72,]$x,data[45721:45792,]$y,type="l",col="green")

plot(result[1:72,]$x,result[1:72,]$y2, type="l", col="red")
lines(result[1:72,]$x,data[45721:45792,]$y2,type="l",col="green")

plot(result[1:72,]$x,result[1:72,]$y3, type="l", col="red")
lines(result[1:72,]$x,data[45721:45792,]$y3,type="l",col="green")

plot(result[1:72,]$x,result[1:72,]$y4, type="l", col="red")
lines(result[1:72,]$x,data[45721:45792,]$y4,type="l",col="green")

plot(result[1:72,]$x,result[1:72,]$y5, type="l", col="red")
lines(result[1:72,]$x,data[45721:45792,]$y5,type="l",col="green")




####
##
## Apply the model to the Anomaly Dataset
##
####


rm(list = ls()) # clear workspace, use if needed

model <- load_model_hdf5("GRU_Predictor_syn", compile = TRUE)

load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset")

data <- data.matrix(data[,-1])

train_data <- data[1:45600,1:5]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset_with_anomalies")

data <- data.matrix(data[,c(-1,-7)])
data[,1:5] <- scale(data[,1:5], center = mean, scale = std)

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
    }            
    
    list(samples)
  }
}


lookback <- 120 # 5d in the past
step <- 1
delay <- 1 # 1h in the future
batch_size <- 0

test_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 48000,
  step = step,
  batch_size = batch_size
)


result <- predict(model, test_gen, steps = 47880, verbose = TRUE)

load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/GRU_Result")

result <- as.data.frame(result)
colnames(result)[1] <- "y"
colnames(result)[2] <- "y2"
colnames(result)[3] <- "y3"
colnames(result)[4] <- "y4"
colnames(result)[5] <- "y5"

result$x <- 1:nrow(result)

load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset_with_anomalies")

data <- data.matrix(data[,c(-1,-7)])
data[,1:5] <- scale(data[,1:5], center = mean, scale = std)

data <- as.data.frame(data)

par(mfrow=c(1,1))
plot(result[1:72,]$x,result[1:72,]$y, type="l", col="red")
lines(result[1:72,]$x,data[122:193,]$y,type="l",col="green")

plot(result[1:72,]$x,result[1:72,]$y2, type="l", col="red")
lines(result[1:72,]$x,data[122:193,]$y2,type="l",col="green")

plot(result[1:72,]$x,result[1:72,]$y3, type="l", col="red")
lines(result[1:72,]$x,data[122:193,]$y3,type="l",col="green")

plot(result[1:72,]$x,result[1:72,]$y4, type="l", col="red")
lines(result[1:72,]$x,data[122:193,]$y4,type="l",col="green")

plot(result[1:72,]$x,result[1:72,]$y5, type="l", col="red")
lines(result[1:72,]$x,data[122:193,]$y5,type="l",col="green")

diff <- sqrt((data[122:nrow(data),]$y - result[1:nrow(result)-1,]$y)^2)
diff2 <- sqrt((data[122:nrow(data),]$y2 - result[1:nrow(result)-1,]$y2)^2)
diff3 <- sqrt((data[122:nrow(data),]$y3 - result[1:nrow(result)-1,]$y3)^2)
diff4 <- sqrt((data[122:nrow(data),]$y4 - result[1:nrow(result)-1,]$y4)^2)
diff5 <- sqrt((data[122:nrow(data),]$y5 - result[1:nrow(result)-1,]$y5)^2)

diff <- as.data.frame(diff)
diff2 <- as.data.frame(diff2)
diff3 <- as.data.frame(diff3)
diff4 <- as.data.frame(diff4)
diff5 <- as.data.frame(diff5)


par(mfrow=c(2,1))
plot(result[18264:18336,]$x,result[18264:18336,]$y, type="l", col="blue", xlab = "Timesteps", ylab = "Predicted vs. Real")
lines(result[18264:18336,]$x,data[18265:18337,]$y,type="l",col="green")


plot(result[18264:18336,]$x,diff[18145:18217,], type = "l", col="red", xlab = "Timesteps", ylab = "Anomaly Score")



par(mfrow=c(2,1))
plot(result[21720:21792,]$x,data[21721:21793,]$y,type="l",col="green", xlab = "Timesteps", ylab = "Predicted vs. Real")
lines(result[21720:21792,]$x,result[21720:21792,]$y, type="l", col="blue")

plot(result[21600:21672,]$x,diff[21600:21672,], type = "l", col="red", xlab = "Timesteps", ylab = "Anomaly Score")


###
#
# Indicate the anomalies
#
###
anomaly <- NULL
anomaly2 <- NULL
anomaly3 <- NULL
anomaly4 <- NULL
anomaly5 <- NULL


for (i in 1:1994) {
  index_end <- i *24
  index_start <- index_end - 23
  anomaly <- rbind(anomaly, mean(diff[index_start:index_end,]))
}
for (i in 1:1994) {
  index_end <- i *24
  index_start <- index_end - 23
  anomaly2 <- rbind(anomaly2, mean(diff2[index_start:index_end,]))
}
for (i in 1:1994) {
  index_end <- i *24
  index_start <- index_end - 23
  anomaly3 <- rbind(anomaly3, mean(diff3[index_start:index_end,]))
}
for (i in 1:1994) {
  index_end <- i *24
  index_start <- index_end - 23
  anomaly4 <- rbind(anomaly4, mean(diff4[index_start:index_end,]))
}
for (i in 1:1994) {
  index_end <- i *24
  index_start <- index_end - 23
  anomaly5 <- rbind(anomaly5, mean(diff5[index_start:index_end,]))
}

anomaly <- cbind(anomaly,anomaly2,anomaly3,anomaly4,anomaly5)
anomaly <- as.data.frame(anomaly)


par(mfrow=c(5,1))
plot(1:1994,anomaly$V1, type = "l", col="red",xlab = "Timesteps", ylab = "Anomaly Score y")
plot(1:1994,anomaly$V2, type = "l", col="red",xlab = "Timesteps", ylab = "Anomaly Score y2")
plot(1:1994,anomaly$V3, type = "l", col="red",xlab = "Timesteps", ylab = "Anomaly Score y3")
plot(1:1994,anomaly$V4, type = "l", col="red",xlab = "Timesteps", ylab = "Anomaly Score y4")
plot(1:1994,anomaly$V5, type = "l", col="red",xlab = "Timesteps", ylab = "Anomaly Score y5")

anomaly.y1 <- subset(anomaly$V1, anomaly$V1 > 0.5)
anomaly.y2 <- subset(anomaly$V2, anomaly$V2 > 0.64)
anomaly.y3 <- subset(anomaly$V3, anomaly$V3 > 0.5)
anomaly.y4 <- subset(anomaly$V4, anomaly$V4 > 0.35) # 2 false positives
anomaly.y5 <- subset(anomaly$V5, anomaly$V5 > 1.3) # one miss








