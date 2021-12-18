rm(list = ls()) # clear workspace, use if needed

load(file = "C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset")

data$anomaly <- 0

# we have 2000 days
# and 48000 data point
# we want between 480 and 1000 anomalou data points
# whole days are anomalies, let's go for 30 days
# 30 days and 5 time series so 6 in each
# if possible 2 types of anomalies are added per variable


#plot(data[1:72,]$x,data[1:72,]$y, type="l")

# anomalies in y
for (i in 1:3){
  index <- floor(runif(1, 0, 2000))
  index <- index * 24
  x <- seq(0,length.out=24)
  anomaly <- jitter(sin(x /2) , factor=300, amount = NULL) + 1 
  data[index:(index+23),]$y <- anomaly
  data[index:(index+23),]$anomaly <- 1
}

#plot(data[4008:4080,]$x,data[4008:4080,]$y, type="l")

for (i in 1:3){
  index <- floor(runif(1, 0, 2000))
  index <- index * 24
  x <- seq(0,length.out=10)
  anom_seq1 <- jitter(x * 0 +4.5 , factor=2, amount = NULL) 
  anom_seq2 <- jitter(x * 0 +4.5 , factor=2, amount = NULL)
  pad <- x <- seq(0,length.out=2)
  anomaly <- c(anom_seq1,pad,anom_seq2,pad)
  data[index:(index+23),]$y <- anomaly
  data[index:(index+23),]$anomaly <- 1
}


#plot(data[25488:25560,]$x,data[25488:25560,]$y, type="l")

# anomalies in y2
for (i in 1:3){
  index <- floor(runif(1, 0, 2000))
  index <- index * 24
  x <- seq(0,length.out=24)
  anomaly <- (jitter(sin(x /4), factor=300, amount = NULL) + 2 ) * 2.5
  data[index:(index+23),]$y2 <- anomaly
  data[index:(index+23),]$anomaly <- 2
}

#plot(data[18984:19056,]$x,data[18984:19056,]$y2, type="l")

for (i in 1:3){
  index <- floor(runif(1, 0, 2000))
  index <- index * 24
  x <- seq(0,length.out=24)
  anomaly <- jitter(sin(x /4), factor=300, amount = NULL) + 2 
  data[index:(index+23),]$y2 <- anomaly
  data[index:(index+23),]$anomaly <- 2
}

#plot(data[19272:19344,]$x,data[19272:19344,]$y2, type="l")

# anomalies in y3
#plot(data[18984:19056,]$x,data[18984:19056,]$y3, type="l")

for (i in 1:3){
  index <- floor(runif(1, 0, 2000))
  index <- index * 24
  data[(index+1):(index+24),]$y3 <- data[(index+1):(index+24),]$y3 + 2
  data[index:(index+23),]$anomaly <- 3
}

#plot(data[12912:12984,]$x,data[12912:12984,]$y3, type="l")

for (i in 1:3){
  index <- floor(runif(1, 0, 2000))
  index <- index * 24
  x <- seq(0,length.out=24)
  data[(index+1):(index+24),]$y3 <- jitter(x * 0 +2.5 , factor=30, amount = NULL)
  data[index:(index+23),]$anomaly <- 3
}

#plot(data[39552:39618,]$x,data[39552:39618,]$y3, type="l")

# anomalies in y4
#plot(data[18984:19056,]$x,data[18984:19056,]$y4, type="l")

for (i in 1:3){
  index <- floor(runif(1, 0, 2000))
  index <- index * 24
  data[(index):(index+11),]$y4 <- sort(runif(12,0,6))
  data[index:(index+23),]$anomaly <- 4
}

#plot(data[1800:1872,]$x,data[1800:1872,]$y4, type="l")

for (i in 1:3){
  index <- floor(runif(1, 0, 2000))
  index <- index * 24
  data[(index+12):(index+23),]$y4 <- sort(runif(12,0,4),decreasing = TRUE)
  data[index:(index+23),]$anomaly <- 4
}

#plot(data[7464:7536,]$x,data[7464:7536,]$y4, type="l")

# anomalies in y5
#plot(data[7464:7536,]$x,data[7464:7536,]$y5, type="l")

for (i in 1:3){
  index <- floor(runif(1, 0, 2000))
  index <- index * 24
  data[(index):(index+23),]$y5 <- runif(24, 0, 0.2) + runif(1,0,2)
  data[index:(index+23),]$anomaly <- 5
}

#plot(data[46008:46080,]$x,data[46008:46080,]$y5, type="l")

for (i in 1:3){
  index <- floor(runif(1, 0, 2000))
  index <- index * 24
  data[(index):(index+23),]$y5 <- runif(24, -1, 3)
  data[index:(index+23),]$anomaly <- 5
}

#plot(data[6576:6648,]$x,data[6576:6648,]$y5, type="l")

summary(data)

#save(data, file ="C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset_train_classifier")
#save(data, file ="C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset_with_anomalies")


load("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset_with_anomalies")


par(mfrow=c(3,1))
plot(data[18264:18336,]$x,data[18264:18336,]$y, type="l", ylab = "y1", xlab = "hours", col = "blue")
plot(data[2952:3024,]$x,data[2952:3024,]$y2, type="l", ylab = "y2", xlab = "hours", col = "blue")
plot(data[721:793,]$x,data[721:793,]$y3, type="l", ylab = "y3", xlab = "hours", col = "blue")



y5.1 <- 7104 /24
y5.2 <- 9792 /24
y5.3 <- 14808 /24
y5.4 <- 33912 /24
y5.5 <- 43752 /24
y5.6 <- 47616 /24

y4.1 <- 9480 /24
y4.2 <- 10104 /24
y4.3 <- 16152 /24
y4.4 <- 23184 /24
y4.5 <- 39672 /24
y4.6 <- 40800 /24

y3.1 <- 744 /24
y3.2 <- 4248 /24
y3.3 <- 6456 /24
y3.4 <- 17040 /24
y3.5 <- 25512 /24
y3.6 <- 28464 /24

y2.1 <- 2976 /24
y2.2 <- 13704 /24
y2.3 <- 20256 /24
y2.4 <- 28752 /24
y2.5 <- 32784 /24
y2.6 <- 40896 /24

y1.1 <- 18288 /24
y1.2 <- 21744 /24
y1.3 <- 22848 /24
y1.4 <- 24744 /24
y1.5 <- 26040 /24
y1.6 <- 40128 /24
