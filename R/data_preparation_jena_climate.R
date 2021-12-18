library(dplyr)
library(lubridate)

rm(list = ls()) # clear workspace, use if needed

source("~/MSCBIS/MT/trunk/R/anomaly_generator.R")

data_dir <- "~/Downloads/jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
data <- read_csv(fname)

data.small <- data.frame(data)

data.small <- data.small %>% select(Date.Time,T..degC.,p..mbar.,wv..m.s.,wd..deg.)
# and only 3456 entries

#the dataset represents sensor data
#we want to simulate a malfunctioning sensor
# 52558 -> 1 year
data.small <- as.data.frame(data.small[1:57024,])#57022
data.small$Anomaly <- FALSE

# temp anomalies
data.small <- embed_anomalies(data.small,2)
data.small <- embed_temp_anomaly(data.small,2)

data.small <- embed_anomalies(data.small,3)

data.small <- embed_anomalies(data.small,4)

data.small <- embed_anomalies(data.small,5)

# count the present anomalies
nrow(data.small[data.small$Anomaly == TRUE,])


# look at the dataset 
g.1 <- ggplot(data.small[1:nrow(data.small),], aes(x=1:nrow(data.small[1:nrow(data.small),]),y=T..degC.)) + geom_line()
g.1

g.2 <- ggplot(data.small[1:nrow(data.small),], aes(x=1:nrow(data.small[1:nrow(data.small),]),y=p..mbar.)) + geom_line()
g.2

g.3 <- ggplot(data.small[1:nrow(data.small),], aes(x=1:nrow(data.small[1:nrow(data.small),]),y=wv..m.s.)) + geom_line()
g.3

g.4 <- ggplot(data.small[1:nrow(data.small),], aes(x=1:nrow(data.small[1:nrow(data.small),]),y=wd..deg.)) + geom_line()
g.4

# there is one anomaly naturally present, let's label it
data.small[4070,4:5] #zero values
data.small[4070,6] <- TRUE

# # if an hour contains an anomaly the whole hour is marked anomalous
# for (i in 1:(nrow(data.small)/6)){
#   n <- i * 6
#   m <- n + 5
#   if(!is.na(match(TRUE,data.small[n:m,6]))){
#     data.small[n:m,6] <- TRUE
#   }
# }



save(data.small, file = "C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/Jena_weather/january_with_anomalies")




