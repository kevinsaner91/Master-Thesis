library(tictoc)
library(keras)
library(tidyverse)

rm(list = ls()) # clear workspace, use if needed

load(file = "../datasets/GHL/GHL_training_data") # length 1535118

data <- data.matrix(data)

train_data <- data[1:1381606,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data[,] <- scale(data[,], center = mean, scale = std)

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
    output5 <- array(0, dim = c(length(rows),delay, 1))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      output1[j,,] <- data[rows[[j]] + delay,1] 
      output2[j,,] <- data[rows[[j]] + delay,2] 
      output3[j,,] <- data[rows[[j]] + delay,3] 
      output4[j,,] <- data[rows[[j]] + delay,4] 
      output5[j,,] <- data[rows[[j]] + delay,5]
      
      array1 <- array(output1, dim = dim(output1))
      array2 <- array(output2, dim = dim(output2))
      array3 <- array(output3, dim = dim(output3))
      array4 <- array(output4, dim = dim(output4))
      array5 <- array(output5, dim = dim(output5))
    }            
    
    list(samples, list(array1, array2, array3, array4, array5))
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

lookback <- 1000 # 5d in the past
step <- 1
delay <- 1 # 1h in the future
batch_size <- 128

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 1381606,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1381606,
  max_index = 1535118,
  step = step,
  batch_size = batch_size
)

val_steps <- (1535118 - 1381606 - lookback) / batch_size

input <- layer_input(shape(lookback,ncol(data)))

model <- input %>% 
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", input_shape = c(lookback,ncol(data))) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_dropout(rate = 0.5) %>%
  layer_conv_1d(filters = 24, kernel_size = 3, activation = "relu", padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_dropout(rate = 0.5) %>%
  layer_conv_1d(filters = 16, kernel_size = 3, activation = "relu", padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten()

output1 <- model %>% layer_dense(units = 1)
output2 <- model %>% layer_dense(units = 1)
output3 <- model %>% layer_dense(units = 1)
output4 <- model %>% layer_dense(units = 1)
output5 <- model %>% layer_dense(units = 1)

model <- keras_model(inputs = input, outputs = c(output1, output2, output3, output4, output5))

model %>% compile(
  optimizer = optimizer_adam(),
  loss = "mae",
  metrics = "mae"
)

summary(model)

tic()
history <- model %>% fit(
  train_gen,
  steps_per_epoch = 200,
  epochs = 8,
  validation_data = val_gen,
  validation_steps = val_steps
)
toc()

# dropout 0.2 -> 1.8022

summary(model)

save_model_hdf5 (model, "CNN_Predictor_GHL", include_optimizer = TRUE)


####
##
## Apply the model to the Anomaly Dataset
##
####

rm(list = ls()) # clear workspace, use if needed

model <- load_model_hdf5("CNN_Predictor_GHL", compile = TRUE)

load("../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23")
data_test <- data
data_test <- subset(data_test, select = c("RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))


load("../datasets/GHL/GHL_training_data")
data_train <- data

data_test <- data.matrix(data_test)
data_train <- data.matrix(data_train)

train_data <- data_train[1:1381606,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

data_test[,1:5] <- scale(data_test[,1:5], center = mean, scale = std)

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


lookback <- 1000 # 5d in the past
step <- 1
delay <- 1 # 1h in the future
batch_size <- 0

test_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 204560,
  step = step,
  batch_size = batch_size
)

tic()
result <- predict(model, test_gen, steps = 204060, verbose = TRUE)
toc()

save(result,file ="../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23_result")


### Evaluate

rm(list = ls()) # clear workspace, use if needed

load("../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23_result")

result <- as.data.frame(result)
colnames(result)[1] <- "RT_level"
colnames(result)[2] <- "RT_temperature.T"
colnames(result)[3] <- "HT_temperature.T"
colnames(result)[4] <- "inj_valve_act"
colnames(result)[5] <- "heater_act"


result$x <- 1:nrow(result)

load("../datasets/GHL/GHL_training_data")
data_train <- data

data_train <- data.matrix(data_train)

train_data <- data_train[1:1381606,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

result[1] <- result[1]*std[1] + mean[1] 
result[2] <- result[2]*std[2] + mean[2] 
result[3] <- result[3]*std[3] + mean[3] 
result[4] <- result[4]*std[4] + mean[4] 
result[5] <- result[5]*std[5] + mean[5] 

load("../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23")

par(mfrow=c(5,1))
plot(result[1:5000,]$x,result[1:5000,]$RT_level, type="l", col="red")
lines(result[1:5000,]$x,data[501:5500,]$RT_level,type="l",col="green")

plot(result[1:5000,]$x,result[1:5000,]$RT_temperature.T, type="l", col="red")
lines(result[1:5000,]$x,data[501:5500,]$RT_temperature.T,type="l",col="green")

plot(result[1:5000,]$x,result[1:5000,]$HT_temperature.T, type="l", col="red")
lines(result[1:5000,]$x,data[501:5500,]$HT_temperature.T,type="l",col="green")

plot(result[1:5000,]$x,result[1:5000,]$inj_valve_act, type="l", col="red")
lines(result[1:5000,]$x,data[501:5500,]$inj_valve_act,type="l",col="green")

plot(result[1:5000,]$x,result[1:5000,]$heater_act, type="l", col="red")
lines(result[1:5000,]$x,data[501:5500,]$heater_act,type="l",col="green")

diff <- sqrt((data[289:3287,]$T1 - result[1:nrow(result)-1,]$T1)^2)
diff2 <- sqrt((data[289:3287,]$T2 - result[1:nrow(result)-1,]$T2)^2)
diff3 <- sqrt((data[289:3287,]$T3 - result[1:nrow(result)-1,]$T3)^2)
diff4 <- sqrt((data[289:3287,]$Appliances - result[1:nrow(result)-1,]$Appliances)^2)


diff <- as.data.frame(diff)
diff2 <- as.data.frame(diff2)
diff3 <- as.data.frame(diff3)
diff4 <- as.data.frame(diff4)

sum_diff <- diff + diff2 + diff3

mean(diff) #0.2712066
mean(diff2) #0.5236905
mean(diff3) #0.3210329
mean(diff4) #29.64146

par(mfrow= c(5,1))
x <- 1:2999
plot(x,diff[x,], type = "l")
lines(x, data[289:3287,]$anomaly, type = "l", col = "red")

plot(x,diff2[x,], type = "l")
lines(x, data[289:3287,]$anomaly, type = "l", col = "red")

plot(x,diff3[x,], type = "l")
lines(x, data[289:3287,]$anomaly, type = "l", col = "red")

plot(x,diff4[x,], type = "l")
lines(x, data[289:3287,]$anomaly, type = "l", col = "red")

x <- 1:2999
plot(x,sum_diff[x,], type = "l")
lines(x, data[289:3287,]$anomaly, type = "l", col = "red")

# Score energy_data_test_with_anomalies_1
anomaly1 <- ifelse(diff > 1.5, TRUE, FALSE) # 2 correct, 1 FP
anomaly2 <- ifelse(diff2 > 3.3, TRUE, FALSE) # 1 correct, 1 FN

# Score energy_data_test_with_anomalies_2
anomaly1 <- ifelse(diff > 1.5, TRUE, FALSE)
#anomaly2 <- ifelse(diff2 > 3.5, TRUE, FALSE) #irrelevant
#anomaly3 <- ifelse(diff3 > )                 #irrelevant
#anomaly_sum <- ifelse(sum_diff > 4.58, TRUE, FALSE)  #irrelevant
# one out of 6 anomalies correctly identified, with one false positive

# Score energy_data_test_with_anomalies_3
anomaly1 <- ifelse(diff > 1.5, TRUE, FALSE)
anomaly2<- ifelse(diff2 > 3.3, TRUE, FALSE)
anomaly_sum <- ifelse(sum_diff > 4.58, TRUE, FALSE) 
# all (3) anomalies are correctly idenfied in the sum variable, no false positives


# Score Total
####################

# 13 Anomalies
# 1 False Positive
# 7 Correctly Classified -> true positives
# 6 Anomalies missed -> False Negatives 

# 375 days -> 375 instances 
375 - 13 # 362 true negatives

precision <- 7/(7+1)
recall <- 7/(7+6)
f1_score <- 2*(precision * recall)/(precision+recall)

########
# Trivial Null Classifier
########

# False negatives 13
# True Positives 0
# False Positives 0
# True Negatives 362

