library(tictoc)
library(keras)
library(tidyverse)
library(ie2misc)

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
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
    }            
    
    list(samples)
  }
}

lookback <- 50
step <- 50
delay <- 1 
batch_size <- 1

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 12000,
  max_index = 12100,
  step = step,
  batch_size = batch_size
)

test_steps <- (12100 - 12000 - lookback) / batch_size

model <- load_model_hdf5("GRU_Predictor", compile = TRUE)

summary(model)

predictions <- model %>% predict(test_gen, steps = test_steps, verbose = TRUE)



