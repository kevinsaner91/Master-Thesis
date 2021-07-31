library(ggplot2)
library(keras)
library(tidyverse)

rm(list = ls()) # clear workspace, use if needed

load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/GHL/train")

data <- data.matrix(data[,-1])
anomaly <- data[,1]

train_data <- data[1:6138450,2:6]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data[,2:6] <- scale(data[,2:6], center = mean, scale = std)

anom <- data[,1]
pmbar <- data[,2]

data[,2] <- anom
data[,1] <- pmbar

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
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] - delay,2] # changed from + to -
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

lookback <- 5000 # 1h in the past
#step <- 200
delay <- 1 # 1/2h in the future
#batch_size <- 10

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 6138450,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 6138450,
  max_index = 6547680,
  step = step,
  batch_size = batch_size
)

val_steps <- (6547680 - 6138450 - lookback) / 128

model <- keras_model_sequential() %>% 
  layer_gru(units = 64,input_shape = list(NULL, dim(data)[[-1]]), return_sequences = TRUE) %>%
  layer_gru(units = 32, return_sequences = TRUE) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = optimizer_adam(),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  train_gen,
  steps_per_epoch = 200,
  epochs = 10,
  validation_data = val_gen,
  validation_steps = val_steps
)



result <- model %>% evaluate(test_gen, steps=  test_steps)




