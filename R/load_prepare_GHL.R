

rm(list = ls()) # clear workspace, use if needed

data <- read.csv("C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/GHL/train_1500000_seed_11_vars_23.csv")

data <- data %>% select(Time,RT_level,RT_temperature.T,inj_valve_act,heater_act)

save(data,file = "C:/Users/Kevin/Documents/MSCBIS/MT/trunk/datasets/GHL/train")

