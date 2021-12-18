library(tictoc)
library(keras)
library(tidyverse)
library(zoo)

rm(list = ls()) # clear workspace, use if needed

load(file = "../../datasets/GHL/GHL_training_data_sample5") # length 1535118

data <- data.matrix(data)

train_data <- data[1:245620,]
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
    
    targets <- array(0, dim = c(length(rows), 2))
    #output2 <- array(0, dim = c(length(rows),delay, 1))
    #output3 <- array(0, dim = c(length(rows),delay, 1))
    #output4 <- array(0, dim = c(length(rows),delay, 1))
    #output5 <- array(0, dim = c(length(rows),delay, 1))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]], 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[j,] <- data[rows[[j]] + delay,c(3,4)] 
      #output2[j,,] <- data[rows[[j]] + delay,4] 
      #output3[j,,] <- data[rows[[j]] + delay,9] 
      #output4[j,,] <- data[rows[[j]] + delay,15] 
      #output5[j,,] <- data[rows[[j]] + delay,16]
      
      #target <- array(output1, dim = dim(output1))
      #array2 <- array(output2, dim = dim(output2))
      #array3 <- array(output3, dim = dim(output3))
      #array4 <- array(output4, dim = dim(output4))
      #array5 <- array(output5, dim = dim(output5))
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

lookback <- 600 # 5d in the past
step <- 1
delay <- 1 # 1h in the future
batch_size <- 128

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 245620,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 245620,
  max_index = 307024,
  step = step,
  batch_size = batch_size
)

val_steps <- (307024 - 245620 - lookback) / batch_size

model <- keras_model_sequential() %>%
  layer_lstm(units = 32,input_shape = list(NULL, ncol(data)), return_sequences = TRUE) %>%
  layer_dropout(rate = 0.1) %>%
  layer_lstm(units = 24) %>%
  layer_dense(units = 2)

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
  epochs = 7,
  validation_data = val_gen,
  validation_steps = val_steps
)
toc()

# dropout 0.2 -> 1.8022

summary(model)

save_model_hdf5 (model, "LSTM_Predictor_GHL_sample5", include_optimizer = TRUE)


####
##
## Apply the model to the Anomaly Dataset
##
####

rm(list = ls()) # clear workspace, use if needed

model <- load_model_hdf5("LSTM_Predictor_GHL_sample5", compile = TRUE)

load("../../datasets/GHL/GHL_test_sample_5")
data_test <- data
#data_test <- subset(data_test, select = c("RT_level", "RT_temperature.T", "HT_temperature.T","inj_valve_act","heater_act"))


load("../../datasets/GHL/GHL_training_data_sample5")
data_train <- data

data_test <- data.matrix(data_test)
data_train <- data.matrix(data_train)

train_data <- data_train[1:245620,]
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


lookback <- 600 # 5d in the past
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

save(result,file ="../../datasets/GHL/GHL_test_LSTM_sample5")


### Evaluate

rm(list = ls()) # clear workspace, use if needed

load("../../datasets/GHL/GHL_test_LSTM_sample5")

result <- as.data.frame(result)
# colnames(result)[1] <- "RT_temperature.T"
# colnames(result)[2] <- "HT_temperature.T"
# colnames(result)[3] <- "RT_level"
# colnames(result)[4] <- "inj_valve_act"
# colnames(result)[5] <- "heater_act"
colnames(result)[1] <- "HT_temperature.T"
colnames(result)[2] <- "RT_level"

result$x <- 1:nrow(result)

load("../../datasets/GHL/GHL_training_data_sample5")
data_train <- data

data_train <- data.matrix(data_train)

train_data <- data_train[1:138160,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)

# result[1] <- result[1]*std[2] + mean[2] 
result[1] <- result[1]*std[3] + mean[3] 
result[2] <- result[2]*std[4] + mean[4] 
# result[4] <- result[4]*std[9] + mean[9] 
# result[5] <- result[5]*std[16] + mean[16] 

load("../../datasets/GHL/GHL_control_sample_5")

par(mfrow=c(2,1))
plot(result[1:20000,]$x,result[1:20000,]$RT_level, type="l", col="red")
lines(result[1:20000,]$x,data[101:20100,]$RT_level,type="l",col="green")

plot(result[1:20000,]$x,data[301:20300,]$DANGER,type="l",col="green")

# plot(result[1:20000,]$x,result[1:20000,]$RT_temperature.T, type="l", col="red")
# lines(result[1:20000,]$x,data[301:20300,]$RT_temperature.T,type="l",col="green")
# 
# plot(result[1:20000,]$x,data[301:20300,]$DANGER,type="l",col="green")

plot(result[1:20000,]$x,result[1:20000,]$HT_temperature.T, type="l", col="red")
lines(result[1:20000,]$x,data[101:20100,]$HT_temperature.T,type="l",col="green")

plot(result[1:20000,]$x,data[101:20100,]$DANGER,type="l",col="black")

# plot(result[1:20000,]$x,result[1:20000,]$inj_valve_act, type="l", col="red")
# lines(result[1:20000,]$x,data[301:20300,]$inj_valve_act,type="l",col="green")
# 
# plot(result[1:20000,]$x,data[301:20300,]$DANGER,type="l",col="green")
# 
# plot(result[1:20000,]$x,result[1:20000,]$heater_act, type="l", col="red")
# lines(result[1:20000,]$x,data[301:20300,]$heater_act,type="l",col="green")
# 
# plot(result[1:20000,]$x,data[301:20300,]$DANGER,type="l",col="green")

#plot(result[1:20156,]$x,data[301:20456,]$DANGER,type="l",col="green")

diff <- sqrt((data[101:980640,]$RT_level - result$RT_level)^2)
#diff2 <- sqrt((data[301:20456,]$RT_temperature.T - result$RT_temperature.T)^2)
diff3 <- sqrt((data[101:980640,]$HT_temperature.T - result$HT_temperature.T)^2)
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


# mean(diff) #0.2712066
# mean(diff2) #0.5236905
# mean(diff3) #0.3210329
# mean(diff4) #29.64146

par(mfrow= c(2,1))
# x <- 1:20156
# plot(x,diff[x,], type = "l")
# lines(x,data[301:20456,]$DANGER,type="l",col="red")
# 
 rollmean <- rollmean(diff,800)

y <- 1:length(rollmean)
plot(y, rollmean, type = "l", ylim = c(0,2))
lines(y,data[900:980640,]$DANGER,type="l",col="red")
abline(h = 0.3, col = "red")


# par(mfrow= c(2,1))
# x <- 1:20156
# plot(x,diff3[x,], type = "l")
# lines(x,data[301:20456,]$DANGER,type="l",col="red")

rollmean3 <- rollmean(diff3,400)

y <- 1:length(rollmean3)
plot(y, rollmean3, type = "l", ylim = c(0,25))
lines(y,data[500:980640,]$DANGER *10,type="l",col="red")
abline(h = 15.5, col = "darkgreen")
abline(h = 11, col = "red")

#################################
##
##  Detailed Results Analysis
##
#################################

indexer_start <- 1
indexer_end <- 20430

par(mfrow= c(2,1)) 
length__ <- 20430

indexer_start <- indexer_start + length__
indexer_end <- indexer_end + length__

y <- indexer_start:indexer_end
plot(y, rollmean[indexer_start:indexer_end], type = "l", ylim = c(0,2))
lines(y,data[900+indexer_start:indexer_end+1,]$DANGER,type="l",col="red")
abline(h = 0.3, col = "red")


plot(y, rollmean3[indexer_start:indexer_end], type = "l", ylim = c(0,25))
lines(y,data[500+indexer_start:indexer_end+1,]$DANGER *10,type="l",col="red")
abline(h = 15.5, col = "darkgreen")
abline(h = 11, col = "red")




## rollmean example plot
par(mfrow= c(2,1))
x <- 500:20156
plot(x,diff3[x,], type = "l", ylab = "Difference", xlab = "Timesteps")
lines(x,data[800:20456,]$DANGER,type="l",col="red")

rollmean3 <- rollmean(diff3,400)

y <- 500:length(rollmean3)
plot(y, rollmean3[500:length(rollmean3)], type = "l", ylim = c(0,25), ylab = "Rolling Mean of Difference", xlab = "Timesteps")
lines(y,data[1001:20478,]$DANGER,type="l",col="red")
abline(h = 15.5, col = "darkgreen")
abline(h = 16, col = "red")


####
#
# Results exp 3.1
#
####

#Overall

# false positives at start not counted
Precision <- 43 /(43 + 4)
Recall <- 43 / (43 + 5)

F1_Score_3.1_LSTM <- 2*(Precision *Recall)/(Precision+Recall)
#####
#unused
rollmean3_df <- as.data.frame(rollmean3)

anomalies <- cbind(rollmean3_df, data[500:980640,]$DANGER)

anomalies <-  subset(anomalies, rowSums(as.data.frame(anomalies$diff3) > 11) > 0)

anomalies


