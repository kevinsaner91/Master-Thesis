rm(list = ls()) # clear workspace, use if needed

load(file = "C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/InterstateTraffic/Metro_Interstate_Traffic")

data$anomaly <- FALSE

27960 /100 * 2


deviant_cycle <- function(data, index_start, index_end){
  x <- seq(0,8*pi,length.out=24)
  y <- ((sin(x) + 2)*500)+3000
  y <- jitter(y, factor=100, amount = NULL)
  data[index_start:index_end,]$traffic_volume <- y
  data[index_start:index_end,]$anomaly <- TRUE
  return(data)
}

deviant_cycle_temp <- function(data, index_start, index_end, offset){
  x <- seq(0,8*pi,length.out=24)
  y <- ((sin(x) + 2)*5)+offset
  y <- jitter(y, factor=100, amount = NULL)
  data[index_start:index_end,]$temp <- y
  data[index_start:index_end,]$anomaly <- TRUE
  return(data)
}

deviant_class_cycle <- function(data, index_start, index_end){
  data[index_start:index_end,]
  
}




index_start <- match(as.POSIXct("2015-12-20 00:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
index_end <-   match(as.POSIXct("2015-12-20 23:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
data <- deviant_cycle(data = data, index_start = index_start, index_end = index_end)
plot(data[(index_start-144):(index_end+144),]$date_time,data[(index_start-144):(index_end+144),]$traffic_volume,type="l")

index_start <- match(as.POSIXct("2018-06-22 00:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
index_end <-   match(as.POSIXct("2018-06-22 23:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
data <- deviant_cycle(data = data, index_start = index_start, index_end = index_end)
plot(data[(index_start-144):(index_end+144),]$date_time,data[(index_start-144):(index_end+144),]$traffic_volume,type="l")

index_start <- match(as.POSIXct("2017-03-02 00:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
index_end <-   match(as.POSIXct("2017-03-02 23:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
data <- deviant_cycle(data = data, index_start = index_start, index_end = index_end)
plot(data[(index_start-144):(index_end+144),]$date_time,data[(index_start-144):(index_end+144),]$traffic_volume,type="l")

index_start <- match(as.POSIXct("2016-02-13 00:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
index_end <-   match(as.POSIXct("2016-02-13 23:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
data <- deviant_cycle_temp(data = data, index_start = index_start, index_end = index_end, 260)
plot(data[(index_start-144):(index_end+144),]$date_time,data[(index_start-144):(index_end+144),]$temp,type="l")

index_start <- match(as.POSIXct("2017-09-10 00:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
index_end <-   match(as.POSIXct("2017-09-10 23:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
data <- deviant_cycle_temp(data = data, index_start = index_start, index_end = index_end, 280)
plot(data[(index_start-144):(index_end+144),]$date_time,data[(index_start-144):(index_end+144),]$temp,type="l")

index_start <- match(as.POSIXct("2016-01-30 00:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
index_end <-   match(as.POSIXct("2016-01-30 23:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
data <- deviant_cycle_temp(data = data, index_start = index_start, index_end = index_end, 260)
plot(data[(index_start-144):(index_end+144),]$date_time,data[(index_start-144):(index_end+144),]$temp,type="l")

index_start <- match(as.POSIXct("2018-01-30 00:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
index_end <-   match(as.POSIXct("2018-01-30 23:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
data <- deviant_cycle_temp(data = data, index_start = index_start, index_end = index_end, 260)
plot(data[(index_start-144):(index_end+144),]$date_time,data[(index_start-144):(index_end+144),]$temp,type="l")







