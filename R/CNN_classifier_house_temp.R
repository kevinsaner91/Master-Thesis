library(ggplot2)
library(keras)
library(tidyverse)
library(tictoc)

rm(list = ls()) # clear workspace, use if needed

load("../datasets/energy_data/energy_data_train_with_anomalies")
data_train <- data_train[!is.na(data_train$T1),]

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
                                dim(data)[[-1]] -1))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,1:10]
      targets[[j]] <- data[rows[[j]] - delay,11] # changed from + to -
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

lookback <- 288 # 1h in the past
step <- 1
delay <- 12 # 1/2h in the future
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
  max_index = 16440,
  step = step,
  batch_size = batch_size
)

val_steps <- (16440 - 13152 - lookback) / batch_size

model <- keras_model_sequential() %>% 
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", input_shape = c(lookback,(ncol(data)-1))) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 16, kernel_size = 3, activation = "relu", padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_upsampling_1d() %>%
  layer_flatten() %>%
  
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = optimizer_adam(),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

tic("start")
history <- model %>% fit(
  train_gen,
  steps_per_epoch = 200,
  epochs = 25,
  validation_data = val_gen,
  validation_steps = val_steps
)
toc()

save_model_hdf5 (model, "CNN_Classifier_house_temp", include_optimizer = TRUE)

####
##
## Apply the model to the Anomaly Dataset
##
####


rm(list = ls()) # clear workspace, use if needed

model <- load_model_hdf5("CNN_Classifier_house_temp", compile = TRUE)


load("../datasets/energy_data/energy_data_test_with_anomalies_1")
load("../datasets/energy_data/energy_data_train_with_anomalies")
data_train <- data_train[!is.na(data_train$T1),]

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
                                dim(data)[[-1]] -1))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,1:10]
      targets[[j]] <- data[rows[[j]] - delay,11] # changed from + to -
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

lookback <- 288 # 1h in the past
step <- 1
delay <- 12 # 1/2h in the future
batch_size <- 0

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 3288,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)


test_steps <- (16440 - 13152 - lookback) / batch_size

tic()
result <- predict(model, test_gen, steps = 3000, verbose = TRUE)
toc()

load("../datasets/energy_data/energy_data_test_with_anomalies_2")

par(mfrow = c(2,1))
x <- 289:3288
plot(x, data_test[x,]$anomaly, type = "l")
plot(x, result[1:3000,], type = "l", col = "red")

