library(tictoc)
library(keras)
library(tidyverse)
library(zoo)

rm(list = ls()) # clear workspace, use if needed

load(file = "../../datasets/GHL/GHL_training_data") # length 1535118

data <- data.matrix(data)

train_data <- data[1:138160,]
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
    # output3 <- array(0, dim = c(length(rows),delay, 1))
    # output4 <- array(0, dim = c(length(rows),delay, 1))
    # output5 <- array(0, dim = c(length(rows),delay, 1))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      output1[j,,] <- data[rows[[j]] + delay,3] 
      output2[j,,] <- data[rows[[j]] + delay,4] 
      # output3[j,,] <- data[rows[[j]] + delay,9] 
      # output4[j,,] <- data[rows[[j]] + delay,15] 
      # output5[j,,] <- data[rows[[j]] + delay,16]
      
      array1 <- array(output1, dim = dim(output1))
      array2 <- array(output2, dim = dim(output2))
      # array3 <- array(output3, dim = dim(output3))
      # array4 <- array(output4, dim = dim(output4))
      # array5 <- array(output5, dim = dim(output5))
    }            
    
    list(samples, list(array1, array2))
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

lookback <- 300 # 5d in the past
step <- 1
delay <- 1 # 1h in the future
batch_size <- 128

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 138160,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 138160,
  max_index = 153512,
  step = step,
  batch_size = batch_size
)

val_steps <- (153512 - 138160 - lookback) / batch_size

input <- layer_input(shape(lookback,ncol(data)))

model <- input %>% 
  layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu", input_shape = c(lookback,ncol(data))) %>%
  layer_dropout(rate = 0.2) %>%
  layer_conv_1d(filters = 48, kernel_size = 3, activation = "relu", padding = "same") %>%
  layer_dropout(rate = 0.2) %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", padding = "same") %>%
  layer_flatten()

output1 <- model %>% layer_dense(units = 1)
output2 <- model %>% layer_dense(units = 1)
# output3 <- model %>% layer_dense(units = 1)
# output4 <- model %>% layer_dense(units = 1)
# output5 <- model %>% layer_dense(units = 1)

model <- keras_model(inputs = input, outputs = c(output1, output2))

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
  epochs = 5,
  validation_data = val_gen,
  validation_steps = val_steps
)
toc()

# loss -> 0.2985 

summary(model)

save_model_hdf5 (model, "CNN_Predictor_GHL_2_timer", include_optimizer = TRUE)


####
##
## Apply the model to the Anomaly Dataset
##
####

rm(list = ls()) # clear workspace, use if needed

model <- load_model_hdf5("CNN_Predictor_GHL_2", compile = TRUE)

load("../../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23")
data_test <- data
#data_test <- subset(data_test, select = c("RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))


load("../../datasets/GHL/GHL_training_data")
data_train <- data

data_test <- data.matrix(data_test)
data_train <- data.matrix(data_train)

train_data <- data_train[1:138160,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

data_test[,] <- scale(data_test[,], center = mean, scale = std)

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


lookback <- 300 # 5d in the past
step <- 1
delay <- 1 # 1h in the future
batch_size <- 0

test_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = dim(data)[[-2]],
  step = step,
  batch_size = batch_size
)

steps <- dim(data)[[-2]] - lookback

tic()
result <- predict(model, test_gen, steps = steps, verbose = TRUE)
toc()

save(result,file ="../../datasets/GHL/01_Lev_fault_Temp_corr_seed_11_vars_23_result_CNN")


### Evaluate

rm(list = ls()) # clear workspace, use if needed

load("../../datasets/GHL/GHL_test_CNN_2")

result <- as.data.frame(result)
# colnames(result)[1] <- "RT_temperature.T"
# colnames(result)[2] <- "RT_level"
# colnames(result)[3] <- "inj_valve_act"
# colnames(result)[4] <- "HT_temperature.T"
# colnames(result)[5] <- "heater_act"
colnames(result)[1] <- "HT_temperature.T"
colnames(result)[2] <- "RT_level"

result$x <- 1:nrow(result)

load("../../datasets/GHL/GHL_training_data")
data_train <- data

data_train <- data.matrix(data_train)

train_data <- data_train[1:138160,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

# result[1] <- result[1]*std[2] + mean[2] 
# result[2] <- result[2]*std[4] + mean[4] 
# result[3] <- result[3]*std[9] + mean[9] 
# result[4] <- result[4]*std[15] + mean[15] 
# result[5] <- result[5]*std[16] + mean[16] 
result[1] <- result[1]*std[3] + mean[3] 
result[2] <- result[2]*std[4] + mean[4] 

load("../../datasets/GHL/GHL_test_control")

par(mfrow=c(2,1))
plot(result[1:10000,]$x,result[1:10000,]$RT_level, type="l", col="red")
lines(result[1:10000,]$x,data[301:10300,]$RT_level,type="l",col="green")

# plot(result[1:5000,]$x,result[1:5000,]$RT_temperature.T, type="l", col="red")
# lines(result[1:5000,]$x,data[301:5300,]$RT_temperature.T,type="l",col="green")

plot(result[1:10000,]$x,result[1:10000,]$HT_temperature.T, ylim = c(300,340),type="l", col="red")
lines(result[1:10000,]$x,data[301:10300,]$HT_temperature.T,type="l",col="green")
# 
# plot(result[1:5000,]$x,result[1:5000,]$inj_valve_act, type="l", col="red")
# lines(result[1:5000,]$x,data[301:5300,]$inj_valve_act,type="l",col="green")
# 
# plot(result[1:5000,]$x,result[1:5000,]$heater_act, type="l", col="red")
# lines(result[1:5000,]$x,data[301:5300,]$heater_act,type="l",col="green")

## Plot for Docu
# par(mfrow=c(1,1))
# plot(result[1:10000,]$x,data[301:10300,]$RT_level,type="l",col="green", ylim = c(0,3), ylab = "RT Level", xlab = "Timesteps")
# lines(result[1:10000,]$x,data[301:10300,]$DANGER,type="l",col="red")
# 
# plot(result[510751:520750,]$x,data[511051:521050,]$HT_temperature.T,type="l",col="green", ylab = "HT Temperature", xlab = "Timesteps")
# lines(result[510751:520750,]$x,data[511051:521050,]$DANGER * 10 + 300,type="l",col="red")


diff <- sqrt((data[301:980640,]$RT_level - result$RT_level)^2)
#diff2 <- sqrt((data[301:20456,]$RT_temperature.T - result$RT_temperature.T)^2)
diff3 <- sqrt((data[301:980640,]$HT_temperature.T - result$HT_temperature.T)^2)
# diff4 <- sqrt((data[301:20456,]$inj_valve_act - result$inj_valve_act)^2)
# diff5 <- sqrt((data[301:20456,]$heater_act - result$heater_act)^2)



# diff2 <- sqrt((data[289:3287,]$T2 - result[1:nrow(result),]$T2)^2)
# diff3 <- sqrt((data[289:3287,]$T3 - result[1:nrow(result),]$T3)^2)
# diff4 <- sqrt((data[289:3287,]$Appliances - result[1:nrow(result)-1,]$Appliances)^2)


diff <- as.data.frame(diff)
#diff2 <- as.data.frame(diff2)
diff3 <- as.data.frame(diff3)
# diff4 <- as.data.frame(diff4)
# diff5 <- as.data.frame(diff5)

sum_diff <- diff * 7.5 +  diff3 

# mean(diff) #0.2712066
# mean(diff2) #0.5236905
# mean(diff3) #0.3210329
# mean(diff4) #29.64146

par(mfrow= c(2,1))
# x <- 1:20156
# plot(x,diff[x,], type = "l")
# lines(x,data[301:20456,]$DANGER,type="l",col="red")
# abline(h = 2, col = "darkgreen")

rollmean <- rollmean(diff,800)

y <- 1:length(rollmean)
plot(y, rollmean, type = "l", ylim = c(0,2))
lines(y,data[1100:980640,]$DANGER,type="l",col="red")
abline(h = 0.4 ,col = "red")

## Plot for Docu
par(mfrow=c(2,1))

y <- 2000:19000
plot(y,diff[y,], type = "l")

plot(y, rollmean[y,], type = "l", ylim = c(0,2))
lines(y,data[y,]$DANGER,type="l",col="red")
abline(h = 0.4 ,col = "red")

# x <- 1:20156
# plot(x,diff3[x,], type = "l")
# lines(x,data[301:20456,]$DANGER,type="l",col="red")

rollmean3 <- rollmean(diff3,400)

y <- 1:length(rollmean3)
plot(y, rollmean3, type = "l", ylim = c(0,18))
lines(y,data[700:980640,]$DANGER *7,type="l",col="red")
abline(h = 9, col = "red")



indexer_start <- 1
indexer_end <- 20430

par(mfrow= c(2,1)) 
length__ <- 20430

indexer_start <- indexer_start + length__
indexer_end <- indexer_end + length__

y <- indexer_start:indexer_end
plot(y, rollmean[indexer_start:indexer_end], type = "l", ylim = c(0,2))
lines(y,data[900+indexer_start:indexer_end+1,]$DANGER,type="l",col="red")
abline(h = 0.4, col = "red")


plot(y, rollmean3[indexer_start:indexer_end], type = "l", ylim = c(0,25))
lines(y,data[500+indexer_start:indexer_end+1,]$DANGER *10,type="l",col="red")
abline(h = 9.5, col = "red")




####
#
# Results experiment 3.1
#
####

#Overall

# false positives at start not counted
Precision <- 40 /(40 + 4)
Recall <- 40 / (40 + 8)

F1_Score_3.1_CNN <- 2*(Precision *Recall)/(Precision+Recall)


rm(list = ls()) # clear workspace, use if needed

load("../../datasets/GHL/GHL_test_control")

x <- 490321:600000
plot(x, data[x,]$HT_temperature.T, type="l", col="green", xlab = "Timesteps", ylab = "HT Temperature")
lines(x,data[x,]$DANGER * 10 + 320,type="l",col="red")
abline(h = 333, col = "black", lwd = 2)

