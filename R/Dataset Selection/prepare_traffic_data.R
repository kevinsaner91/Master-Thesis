library(dplyr)
library(fastDummies)
library(ggplot2)
library(vcd)

# holiday Categorical US National holidays plus regional holiday, Minnesota State Fair
# temp Numeric Average temp in kelvin
# rain_1h Numeric Amount in mm of rain that occurred in the hour
# snow_1h Numeric Amount in mm of snow that occurred in the hour
# clouds_all Numeric Percentage of cloud cover
# weather_main Categorical Short textual description of the current weather
# weather_description Categorical Longer textual description of the current weather
# date_time DateTime Hour of the data collected in local CST time
# traffic_volume Numeric Hourly I-94 ATR 301 reported westbound traffic volume


rm(list = ls()) # clear workspace, use if needed

setwd("~/MSCBIS/MT/trunk/datasets/InterstateTraffic")

data <- read.csv("Metro_Interstate_Traffic_Volume.csv")

data$date_time <- as.POSIXct(data$date_time,format="%Y-%m-%d %H:%M:%S",,tz=Sys.timezone())

data <- data[match(as.POSIXct("2015-06-26 00:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time):nrow(data),]
rownames(data) <- 1:nrow(data) # reindex

data <- distinct(data, data$date_time, .keep_all = TRUE)

# data$date_time[7] + 604800
# 
# #2012-10-03 06:00:00
# 
# match(as.POSIXct("2012-10-03 07:00:00",format="%Y-%m-%d %H:%M:%S",,tz=Sys.timezone()),data$date_time)

for(k in 1:12){
  j <- 0
  n <- nrow(data) -23 #minus last day
  for (i in 1:n) {
    row <- data[i,]
    row_next <- row$date_time + 3600 #plus 1 hour
    if(is.na(match(as.POSIXct(row_next,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time))){
      
      next_day <- row$date_time + 90000 #plus one day
      index <- match(as.POSIXct(next_day,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)
      
      if(!is.na(index)){
        sub <- data[index,]
        sub$date_time <- row_next
        data <- rbind(data, sub)
      }else{
        sub <- data[i+24,]
        sub$date_time <- row_next
        data <- rbind(data,sub)
      }
    }  
  }
  data <- data[order(data$date_time, decreasing = FALSE),]
  rownames(data) <- 1:nrow(data) # reindex
}

# remove week after 27.10 - too many missing values
data <- data[-c(match(as.POSIXct("2015-10-05 00:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time):
               match(as.POSIXct("2015-11-01 23:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time)),]

data <- data[1:match(as.POSIXct("2018-09-30 23:00:00",format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time),]

#check for missing values
for (i in 1:n) {
  row <- data[i,]
  row_next <- row$date_time + 3600 #plus 1 hour
  if(is.na(match(as.POSIXct(row_next,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone()),data$date_time))){
    print(row_next)
  }
}
# # entfernen 24.12.2012 - 06.01.2013
# #           23.12.2013 - 05.01.2014
# #           22.12.2014 - 04.01.2015 not there
# #           21.12.2015 - 03.01.2016
# #           19.12.2016 - 01.01.2017
# #           25.12.2017 - 07.01.2018
# 
# index_start <- match("2015-12-21 01:00:00",data$date_time)
# index_end <- match("2016-01-03 00:00:00", data$date_time )
# data <- data[-c(index_start:index_end),]
# 
# index_start <- match("2016-12-19 00:00:00",data$date_time)
# index_end <- match("2017-01-01 00:00:00", data$date_time )
# data <- data[-c(index_start:index_end),]
# 
# index_start <- match("2017-12-25 00:00:00",data$date_time)
# index_end <- match("2018-01-07 00:00:00", data$date_time )
# data <- data[-c(index_start:index_end),]

data$holiday <- as.factor(ifelse(data$holiday != "None", 1, 0))

#

summary(data)

View(data)

data.plot <- data[1:336,] # first 2 weeks

g.1 <- ggplot(data.plot, aes(x=1:nrow(data.plot),y=traffic_volume)) + geom_line()
g.1

data.plot <- data[336:672,] # second 2 weeks

g.1 <- ggplot(data.plot, aes(x=1:nrow(data.plot),y=traffic_volume)) + geom_line()
g.1

g.2 <- ggplot(data.plot, aes(x=1:nrow(data.plot),y=temp)) + geom_line()
g.2

g.3 <- ggplot(data.plot, aes(x=1:nrow(data.plot),y=clouds_all)) + geom_line()
g.3

g.4 <- ggplot(data.plot, aes(x=1:nrow(data.plot),y=rain_1h)) + geom_line()
g.4

barplot(table(data$weather_description[336:672]))

#data <- dummy_cols(data,select_columns = c("weather_main","weather_description"))

data <- data[-10]

data$temp <- smooth(data$temp)
data$traffic_volume <- smooth(data$traffic_volume)
data$rain_1h <- smooth(data$rain_1h)
data$snow_1h <- smooth(data$snow_1h)
data$clouds_all <- smooth(data$clouds_all)

data$weather_main <- ifelse(data$clouds_all < 2, "Clear", data$weather_main)
data$weather_description <- ifelse(data$weather_main == "Clear", "sky is clear", data$weather_description)

data$weather_main <- as.factor(data$weather_main)
data$weather_description <- as.factor(data$weather_description)

save(data, file ="C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/InterstateTraffic/Metro_Interstate_Traffic")

