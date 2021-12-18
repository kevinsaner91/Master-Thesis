rm(list = ls()) # clear workspace, use if needed
art_no_anomaly.1 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialNoAnomaly\\art_daily_no_noise.csv",header = TRUE, sep = ",",)
art_no_anomaly.2 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialNoAnomaly\\art_daily_small_noise.csv",header = TRUE, sep = ",",)
art_no_anomaly.3 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialNoAnomaly\\art_daily_perfect_square_wave.csv",header = TRUE, sep = ",",)
#art_no_anomaly.4 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialNoAnomaly\\art_flatline.csv",header = TRUE, sep = ",",)
#art_no_anomaly.5 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialNoAnomaly\\art_noisy.csv",header = TRUE, sep = ",",)


#art_with_anomaly.1 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialWithAnomaly\\art_daily_flatmiddle.csv",header = TRUE, sep = ",",)
#art_with_anomaly.2 = read.csv("D:\\dev\\Master Thesis Proposal\\root\\datasets\\data_nab\\artificialWithAnomaly\\art_increase_spike_density.csv",header = TRUE, sep = ",",)



multivar<- data.frame(art_no_anomaly.1, art_no_anomaly.3$value, art_no_anomaly.2$value)

g.1 <- ggplot(art_no_anomaly.1, aes(x=1:nrow(art_no_anomaly.1),y=value)) + geom_line()
g.1

g.2 <- ggplot(art_no_anomaly.2, aes(x=1:nrow(art_no_anomaly.2),y=value)) + geom_line()
g.2

g.3 <- ggplot(art_no_anomaly.3, aes(x=1:nrow(art_no_anomaly.3),y=value)) + geom_line()
g.3