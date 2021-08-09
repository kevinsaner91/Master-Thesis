library(tictoc)
library(keras)
library(tidyverse)

rm(list = ls()) # clear workspace, use if needed

setwd("~/MSCBIS/MT/trunk/R")

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

lookback <- 120 # 5d in the past
step <- 1
delay <- 1 # 1h in the future
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

input <- layer_input(shape(lookback,ncol(data)))

model <- input %>% 
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", input_shape = c(lookback,ncol(data))) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", padding = "same") %>%
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

history <- model %>% fit(
  train_gen,
  steps_per_epoch = 200,
  epochs = 10,
  validation_data = val_gen,
  validation_steps = val_steps
)

summary(model)

save_model_hdf5 (model, "CNN_Predictor_syn", include_optimizer = TRUE)

####
##
## Just for testing
##
####

#rm(list = ls()) # clear workspace, use if needed

#model <- load_model_hdf5("CNN_Predictor_syn", compile = TRUE)


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
batch_size <- 128

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

out <- predict(model, test_gen, steps = test_steps, verbose = TRUE)

result <- as.data.frame(out)
colnames(result)[1] <- "y"
colnames(result)[2] <- "y2"
colnames(result)[3] <- "y3"
colnames(result)[4] <- "y4"
colnames(result)[5] <- "y5"

result$x <- 1:nrow(result)


plot(result[1:72,]$x,result[1:72,]$y, type="l")
plot(result[1:72,]$x,result[1:72,]$y2, type="l")
plot(result[1:72,]$x,result[1:72,]$y3, type="l")
plot(result[1:72,]$x,result[1:72,]$y4, type="l")
plot(result[1:72,]$x,result[1:72,]$y5, type="l")




loss <- model %>% evaluate(test_gen, steps = test_steps, verbose = TRUE)

denorm_loss = loss * std


save_model_hdf5 (model, "CNN_Predictor", include_optimizer = TRUE)








