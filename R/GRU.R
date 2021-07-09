library(ggplot2)
library(keras)

setwd("D:\\dev\\Master Thesis Proposal\\root\R")

rm(list = ls()) # clear workspace, use if needed
art_no_anomaly.1 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialNoAnomaly\\art_daily_no_noise.csv",header = TRUE, sep = ",",)
art_no_anomaly.2 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialNoAnomaly\\art_daily_small_noise.csv",header = TRUE, sep = ",",)
art_no_anomaly.3 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialNoAnomaly\\art_daily_perfect_square_wave.csv",header = TRUE, sep = ",",)
#art_no_anomaly.4 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialNoAnomaly\\art_flatline.csv",header = TRUE, sep = ",",)
#art_no_anomaly.5 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialNoAnomaly\\art_noisy.csv",header = TRUE, sep = ",",)


#art_with_anomaly.1 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialWithAnomaly\\art_daily_flatmiddle.csv",header = TRUE, sep = ",",)
#art_with_anomaly.2 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialWithAnomaly\\art_increase_spike_density.csv",header = TRUE, sep = ",",)



multivar<- data.frame(art_no_anomaly.1, art_no_anomaly.3$value, art_no_anomaly.2$value)

g.1 <- ggplot(art_no_anomaly.1, aes(x=1:nrow(art_no_anomaly.1),y=value)) + geom_line()
g.1

g.2 <- ggplot(art_no_anomaly.2, aes(x=1:nrow(art_no_anomaly.2),y=value)) + geom_line()
g.2

g.3 <- ggplot(art_no_anomaly.3, aes(x=1:nrow(art_no_anomaly.3),y=value)) + geom_line()
g.3

data <- data.matrix(multivar[,-1])

train_data <- data[1:3456,]
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
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
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

lookback <- 288
step <- 6
delay <- 5
batch_size <- 36

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 3456,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 3456,
  max_index = 4032,
  step = step,
  batch_size = batch_size
)

val_steps <- (4032 - 3456 - lookback) / batch_size


model <- keras_model_sequential() %>% 
  layer_gru(units = 32,input_shape = list(NULL, dim(data)[[-1]])) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_adam(),
  loss = "mae"
)



history <- model %>% fit(
  train_gen,
  steps_per_epoch = 500,
  epochs = 10,
  validation_data = val_gen,
  validation_steps = val_steps
)



