

data <- NULL

for (i in 1:2000) {
  x <- seq(0,length.out=24)
  y <- (jitter(sin(x /2) , factor=300, amount = NULL) + 1 ) * 2
  y2 <-(jitter(sin(x /4), factor=300, amount = NULL) + 2 ) * 2
  y3 <- jitter(x/4, factor = 20, amount = NULL)
  y4 <- jitter(ifelse(x <12, 5, 0), factor = 1, amount = NULL)
  y5 <- runif(24, 1 , 2)
  
  temp <- cbind(x,y,y2,y3,y4,y5)
  data <- rbind(data,temp)
}

data <- as.data.frame(data)

data$x <- 1:nrow(data)

par(mfrow=c(5,1))
plot(data[1:96,]$x,data[1:96,]$y, type="l", ylab = "y1", xlab = "hours", col = "blue")
plot(data[1:96,]$x,data[1:96,]$y2, type="l", ylab = "y2", xlab = "hours", col = "blue")
plot(data[1:96,]$x,data[1:96,]$y3, type="l", ylab = "y3", xlab = "hours", col = "blue")
plot(data[1:96,]$x,data[1:96,]$y4, type="l" ,ylab = "y4", xlab = "hours", col = "blue")
plot(data[1:96,]$x,data[1:96,]$y5, type="l", ylab = "y5", xlab = "hours", col = "blue")


save(data, file ="C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/SyntheticData/synthetic_dataset")





