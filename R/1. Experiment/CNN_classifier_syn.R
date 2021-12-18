library(ggplot2)
library(keras)
library(tidyverse)
library(tictoc)
library(MLmetrics)

rm(list = ls()) # clear workspace, use if needed

load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset_train_classifier")

data$anomaly <- ifelse(data$anomaly > 0, TRUE, FALSE)

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
                                dim(data)[[-1]] -1))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,1:5]
      targets[[j]] <- data[rows[[j]] - delay,6] # changed from + to -
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

lookback <- 120 # 1h in the past
step <- 1
delay <- 12 # 1/2h in the future
batch_size <- 128

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 45600,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 45600,
  max_index = 48000,
  step = step,
  batch_size = batch_size
)

val_steps <- (48000 - 45600 - lookback) / batch_size

model <- keras_model_sequential() %>% 
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", input_shape = c(lookback,ncol(data)-1)) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
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
  epochs = 30,
  validation_data = val_gen,
  validation_steps = val_steps
)
toc()

save_model_hdf5 (model, "CNN_Classifier_syn", include_optimizer = TRUE)

####
##
## Apply the model to the Anomaly Dataset
##
####


rm(list = ls()) # clear workspace, use if needed

model <- load_model_hdf5("CNN_Classifier_syn", compile = TRUE)

load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset_train_classifier")

train_data <- data[1:45600,1:5]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset_with_anomalies")

data$anomaly <- ifelse(data$anomaly > 0, TRUE, FALSE)

data <- data.matrix(data[,c(-1)])
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
                                dim(data)[[-1]] -1 ))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,1:5]
    }            
    
    list(samples)
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

lookback <- 120 # 1h in the past
step <- 1
delay <- 12 # 12 hours in the past
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

tic()
result <- predict(model, test_gen, steps = 47880, verbose = TRUE)
toc()

save(result,file ="C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/CNN_Classifier_result")

rm(list = ls()) # clear workspace, use if needed

load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset_with_anomalies")
load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/CNN_Classifier_result")
data$anomaly <- ifelse(data$anomaly > 0, TRUE, FALSE)

data <- data[121:48000,]

anomaly_predicted <- NULL
for (i in 1:1994) {
  index_end <- i *24
  index_start <- index_end - 23
  anomaly_predicted <- rbind(anomaly_predicted, mean(result[index_start:index_end,]))
}

anomaly_predicted <- as.data.frame(anomaly_predicted)

anomaly_actual <- NULL
for (i in 1:1994) {
  index_end <- i *24
  index_start <- index_end - 23
  anomaly_actual <- rbind(anomaly_actual, mean(data[index_start:index_end,7]))
}

anomaly_actual <- as.data.frame(anomaly_actual)

anomaly_actual <- ifelse(anomaly_actual > 0.5, TRUE, FALSE)
anomaly_predicted <- ifelse(anomaly_predicted > 0.04, TRUE, FALSE)

sum(anomaly_predicted == 1)
sum(anomaly_actual == 1)

par(mfrow=c(2,1))
plot(1:1994,anomaly_predicted[,], type = "l", col="red",xlab = "Timesteps", ylab = "Anomaly Score Predicted")
line(1:1994,anomaly_actual[,], type = "l", col="red",xlab = "Timesteps", ylab = "Anomaly Score Actual")

anomalies <- cbind(anomaly_actual, anomaly_predicted)


trivial_null_classifier <- 1:1994
trivial_null_classifier[1:1994] <- FALSE
trivial_null_classifier <- as.logical(trivial_null_classifier)


F1_Score(anomaly_actual,anomaly_predicted)
F1_Score(anomaly_actual,trivial_null_classifier) 


# Accuracy

# Predictions 
total <- 1994

trivial_null_correct <- 1994 - 30

cnn_super_correct <- 1994 - 20 


accuracy_trivial_null <- trivial_null_correct / total

accuracy_cnn_super <- cnn_super_correct / total


cnn_unsuper_correct <- 1994 - 2 


accuracy_cnn_unsuper <- cnn_unsuper_correct / total


rnn_unsuper_correct <- 1994 - 3 


accuracy_rnn_unsuper <- rnn_unsuper_correct / total
