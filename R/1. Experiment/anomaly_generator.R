

# point anomaly
point_anomaly <- function(min,max,positive,factor = 0.1){
  x <- runif(1,min = min, max = max)
  if(positive == TRUE){
    max + abs(x*0.1)
  }else{
    min - abs(x*0.1)
  }
}

#helper function for collective anomalies
collective_anomaly <- function(param1,param2){
  if(param1 < param2){
    runif(1,min = param1, max = param2)
  }else{
    runif(1,min = param2, max = param1)
  }
}

#contextual anomaly
temp_anomaly <- function(date){
  dateIn <- as.Date(date, "%d.%m.%Y ") 
  if(month(dateIn) > 9 || month(dateIn) < 3){
    #make it hot
    runif(1,20,30)
  }else{
    #make it cold
    runif(1,0,10)*-1
  }
}

embed_anomalies <- function(data, data_index, point_anomaly = TRUE, collective_anomaly = TRUE, noise = TRUE){
  if(point_anomaly){
    min <- min(data[data_index])
    max <- max(data[data_index])
    index <- sample(1:nrow(data),25)
    for(i in 1:25) {
      data[index[i],data_index] <- point_anomaly(min,max,TRUE)
      data[index[i],6] <- TRUE
    }
    index <- sample(1:nrow(data),25)
    for(i in 1:25) {
      data[index[i],data_index] <- point_anomaly(min,max,FALSE)
      data[index[i],6] <- TRUE
    }
  }
  if(collective_anomaly){
    index <- sample(1:nrow(data),25)
    for(i in 1:25){
      n <- sample(7:15,1)
      data[index[i]:(index[i]+n),data_index] <- collective_anomaly(data[index[i],data_index],data[(index[i]+n),data_index])
      data[(index[i]+1):(index[i]+n),6] <- TRUE
    }
  }
  if(noise)
    index <- sample(1:nrow(data),25)
  for(i in 1:25){
    n <- sample(10:50,1)
    data[index[i]:(index[i]+n),data_index] <- jitter(data[index[i]:(index[i]+n),data_index], factor = 1000, amount = NULL)
    data[(index[i]+1):(index[i]+n),6] <- TRUE
  }
  return(data)
}

embed_temp_anomaly <- function(data, data_index){
  index <- sample(1:nrow(data),50)
  for(i in 1:50) {
    data[index[i],data_index] <- temp_anomaly(data[index[i],1])
    data[index[i],6] <- TRUE
  }
  return(data)
}
