rm(list = ls()) # clear workspace, use if needed

setwd("~/MSCBIS/MT/trunk/datasets/GHL")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

data <- rbind(myfiles[[1]],
              myfiles[[2]],
              myfiles[[3]],
              myfiles[[4]],
              myfiles[[5]],
              myfiles[[6]],
              myfiles[[7]],
              myfiles[[8]],
              myfiles[[9]],
              myfiles[[10]],
              myfiles[[11]],
              myfiles[[12]],
              myfiles[[13]],
              myfiles[[14]],
              myfiles[[15]],
              myfiles[[16]],
              myfiles[[17]],
              myfiles[[18]],
              myfiles[[19]],
              myfiles[[20]],
              myfiles[[21]],
              myfiles[[22]],
              myfiles[[23]],
              myfiles[[24]],
              myfiles[[25]],
              myfiles[[26]],
              myfiles[[27]],
              myfiles[[28]],
              myfiles[[29]],
              myfiles[[30]],
              myfiles[[31]],
              myfiles[[32]],
              myfiles[[33]],
              myfiles[[34]],
              myfiles[[35]],
              myfiles[[36]],
              myfiles[[37]],
              myfiles[[38]],
              myfiles[[39]],
              myfiles[[40]],
              myfiles[[41]],
              myfiles[[42]],
              myfiles[[43]],
              myfiles[[44]],
              myfiles[[45]],
              myfiles[[46]],
              myfiles[[47]],
              myfiles[[48]]
)

data <- data %>% select(Time ,DANGER,inj_valve_act,RT_level,RT_temperature.T,HT_temperature.T,heater_act, HT_level)

save(data,file = "C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/GHL/train")

index_start <- match(1, data$DANGER)
index_end <- 48895

data2 <- data$RT_level[48648:48995]

df <- as.data.frame(data2)


g.1 <- ggplot(df, aes(x=1:nrow(df),y=data2)) + geom_line()
g.1



