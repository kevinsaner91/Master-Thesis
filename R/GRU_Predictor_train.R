library(tictoc)
library(keras)
library(tidyverse)

rm(list = ls()) # clear workspace, use if needed

load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/GHL/train")

data <- data.matrix(data)

max_index <- nrow(data) # just to remember the length

data <- data[,2:5]

train_data <- data[1:10000,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

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
    targets <- array(0, dim = c(length(rows),delay, dim(data)[-1]))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[j,,] <- data[rows[[j]] + delay,] 
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
#* `step` -- The period, in timesteps, at which you sample data.

lookback <- 50
step <- 1
delay <- 1 
batch_size <- 1

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
  min_index = 10000,
  max_index = 12000,
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 12000,
  max_index = 12051,
  step = step,
  batch_size = batch_size
)

val_steps <- (12000 - 10000 - lookback) / batch_size
test_steps <- (12051 - 12000 - lookback) / 1

model <- keras_model_sequential() %>% 
  layer_gru(units = 32,input_shape = list(NULL, ncol(data)), return_sequences = TRUE) %>%
  layer_gru(units = 32, return_sequences = TRUE) %>%
  layer_dense(units = 4)

model %>% compile(
  optimizer = optimizer_adam(),
  loss = "mae",
)

history <- model %>% fit(
  train_gen,
  steps_per_epoch = 200,
  epochs = 10,
  validation_data = val_gen,
  validation_steps = val_steps
)

save_model_hdf5 (model, "GRU_Predictor", include_optimizer = TRUE)








