install.packages("cloudml")

library(cloudml)
gcloud_install()

cloudml_train("GRU.R")
