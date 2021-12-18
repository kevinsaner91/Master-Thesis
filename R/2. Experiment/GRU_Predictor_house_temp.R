library(tictoc)
library(keras)
library(tidyverse)
library(tensorflow)

rm(list = ls()) # clear workspace, use if needed

load(file = "../datasets/energy_data/energy_data_train")


data <- data.matrix(data_train)

train_data <- data[1:13152,1:10]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data[,1:10] <- scale(data[,1:10], center = mean, scale = std)

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
    
    output1 <- array(0, dim = c(length(rows),delay, 1))
    output2 <- array(0, dim = c(length(rows),delay, 1))
    output3 <- array(0, dim = c(length(rows),delay, 1))
    output4 <- array(0, dim = c(length(rows),delay, 1))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      output1[j,,] <- data[rows[[j]] + delay,1] 
      output2[j,,] <- data[rows[[j]] + delay,2] 
      output3[j,,] <- data[rows[[j]] + delay,3] 
      output4[j,,] <- data[rows[[j]] + delay,6]
      
      array1 <- array(output1, dim = dim(output1))
      array2 <- array(output2, dim = dim(output2))
      array3 <- array(output3, dim = dim(output3))
      array4 <- array(output4, dim = dim(output4))
    }            
    
    list(samples, list(array1, array2, array3, array4))
  }
}

#Now here is the data generator you'll use. It yields a list `(samples, targets)`, where `samples` is one batch of input data and `targets` is the corresponding array of target temperatures. It takes the following arguments:

#* `data` -- The original array of floating-point data, which you normalized in listing 6.32.
#* `lookback` -- How many timesteps back the input data should go.
#* `delay` -- How many timesteps in the future the target should be.
#* `min_index` and `max_index` -- Indices in the `data` array that delimit which timesteps to draw from. This is useful for keeping a segment of the data for validation and another for testing.
#* `shuffle` -- Whether to shuffle the samples or draw them in chronological order.
#* `batch_size` -- The number of samples per batch.
#* `step` -- The period, in timesteps, at which you sample data.

lookback <- 288 # 5d in the past
step <- 1
delay <- 1 # 1h in the future
batch_size <- 128

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 13152,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 13152,
  max_index = 16438,
  step = step,
  batch_size = batch_size
)

val_steps <- (16438 - 13152 - lookback) / batch_size

# model <- keras_model_sequential() %>%
#   layer_gru(units = 64,input_shape = list(NULL, ncol(data)), return_sequences = TRUE) %>%
#   layer_gru(units = 32, return_sequences = TRUE) %>%
#   layer_gru(units = 16) %>%
#   layer_dense(units = 4)

model <- keras_model_sequential() %>%
  layer_lstm(units = 64,input_shape = list(NULL, ncol(data)), return_sequences = TRUE) %>%
  layer_lstm(units = 32, return_sequences = TRUE) %>%
  layer_lstm(units = 16) %>%
  layer_dense(units = 4)
  

model %>% compile(
  optimizer = optimizer_adam(),
  loss = "mae",
  metrics = "mae"
)

history <- model %>% fit(
  train_gen,
  steps_per_epoch = 200,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)

summary(model)

loss <- model %>% evaluate(test_gen, steps = test_steps, verbose = TRUE)



save_model_hdf5 (model, "LSTM_Predictor_house_temp", include_optimizer = TRUE)


####
##
## Use the model to make predictions
##
####


rm(list = ls()) # clear workspace, use if needed

model <- load_model_hdf5("LSTM_Predictor_house_temp", compile = TRUE)

load("../datasets/energy_data/energy_data_test_with_anomalies_1")
load("../datasets/energy_data/energy_data_train")

data_test <- data.matrix(data_test)
data_train <- data.matrix(data_train)

train_data <- data_train[1:13152,1:10]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

data_test[,1:10] <- scale(data_test[,1:10], center = mean, scale = std)

data <- data_test

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


lookback <- 288 # 5d in the past
step <- 1
delay <- 1 # 1h in the future
batch_size <- 0

test_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 3288,
  step = step,
  batch_size = batch_size
)

tic()
result <- predict(model, test_gen, steps = 3000, verbose = TRUE)
toc()

save(result,file ="../datasets/energy_data/LSTM_result_house_temp")


### Evaluate


rm(list = ls()) # clear workspace, use if needed

load("../datasets/energy_data/GRU_result_house_temp")

result <- as.data.frame(result)
colnames(result)[1] <- "T1"
colnames(result)[2] <- "T2"
colnames(result)[3] <- "T3"
colnames(result)[4] <- "Appliances"

result$x <- 1:nrow(result)

load("../datasets/energy_data/energy_data_train")
data_train <- data_train[!is.na(data_train$T1),]

data_train <- data.matrix(data_train)

train_data <- data_train[1:13152,1:10]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

result[1] <- result[1]*std[1] + mean[1] 
result[2] <- result[2]*std[2] + mean[2] 
result[3] <- result[3]*std[3] + mean[3] 
result[4] <- result[4]*std[6] + mean[6] 

load("../datasets/energy_data/energy_data_test_with_anomalies_1")

row.names(data_test) <- 1:nrow(data_test)
data <- as.data.frame(data_test)

par(mfrow=c(4,1))
plot(result[1:576,]$x,result[1:576,]$T1, type="l", col="red")
lines(result[1:576,]$x,data[289:864,]$T1,type="l",col="green")

plot(result[1:576,]$x,result[1:576,]$T2, type="l", col="red")
lines(result[1:576,]$x,data[289:864,]$T2,type="l",col="green")

plot(result[1:576,]$x,result[1:576,]$T3, type="l", col="red")
lines(result[1:576,]$x,data[289:864,]$T3,type="l",col="green")

plot(result[1:576,]$x,result[1:576,]$Appliances, type="l", col="red")
lines(result[1:576,]$x,data[289:864,]$Appliances,type="l",col="green")


plot(result[2200:2600,]$x,result[2200:2600,]$T2, type="l", col="red")
lines(result[2200:2600,]$x,data[2489:2889,]$T2,type="l",col="green")

diff <- sqrt((data[289:3287,]$T1 - result[1:nrow(result)-1,]$T1)^2)
diff2 <- sqrt((data[289:3287,]$T2 - result[1:nrow(result)-1,]$T2)^2)
diff3 <- sqrt((data[289:3287,]$T3 - result[1:nrow(result)-1,]$T3)^2)
diff4 <- sqrt((data[289:3287,]$Appliances - result[1:nrow(result)-1,]$Appliances)^2)

mean(diff) #0.1741582
mean(diff2) #0.9009765
mean(diff3) #0.7244096
mean(diff4) #72.91079

diff <- as.data.frame(diff)
diff2 <- as.data.frame(diff2)
diff3 <- as.data.frame(diff3)
diff4 <- as.data.frame(diff4)

sum_diff <- diff  + diff2  + diff3

par(mfrow= c(5,1))
x <- 1:2999
plot(x,diff[x,], type = "l")
lines(x, data[289:3287,]$anomaly, type = "l", col = "red")
x2 <- rep(1, 2999)
lines(x, x2 , type = "l", col = "blue")

plot(x,diff2[x,], type = "l")
lines(x, data[289:3287,]$anomaly, type = "l", col = "red")
x2 <- rep(3.5, 2999)
lines(x, x2 , type = "l", col = "blue")

plot(x,diff3[x,], type = "l")
lines(x, data[289:3287,]$anomaly, type = "l", col = "red")

plot(x,diff4[x,], type = "l")
lines(x, data[289:3287,]$anomaly, type = "l", col = "red")

x <- 1:2999
plot(x,sum_diff[x,], type = "l")
lines(x, data[289:3287,]$anomaly, type = "l", col = "red")
x2 <- rep(6.2, 2999)
lines(x, x2 , type = "l", col = "blue")


# Score energy_data_test_with_anomalies_1
anomaly1 <- ifelse(diff > 1.1, TRUE, FALSE) # 2 correct, 1 FP
anomaly2 <- ifelse(diff2 > 3.5, TRUE, FALSE) # 2 FN or 1 TP and 12 FP

# Score energy_data_test_with_anomalies_2
anomaly1 <- ifelse(diff > 1.0, TRUE, FALSE)
#anomaly2 <- ifelse(diff2 > 3.5, TRUE, FALSE) #irrelevant
#anomaly3 <- ifelse(diff3 > )                 #irrelevant
#anomaly_sum <- ifelse(sum_diff > 4.58, TRUE, FALSE)  #irrelevant
# 3 out of 6 anomalies correctly identified, with one false positive

# Score energy_data_test_with_anomalies_3
anomaly1 <- ifelse(diff > 1.5, TRUE, FALSE)
anomaly2<- ifelse(diff2 > 3.3, TRUE, FALSE)
anomaly_sum <- ifelse(sum_diff > 6.2, TRUE, FALSE) 
# all (3) anomalies are correctly idenfied in the sum variable, no false positives


# Score Total
####################

# 13 Anomalies
# 1 False Positive
# 8 Correctly Classified -> true positives
# 5 Anomalies missed -> False Negatives 

# 375 days -> 375 instances 
375 - 13 # 362 true negatives

precision <- 8/(8+1)
recall <- 8/(8+5)
f1_score <- 2*(precision * recall)/(precision+recall)

########
# Trivial Null Classifier
########

# False negatives 13
# True Positives 0
# False Positives 0
# True Negatives 362





