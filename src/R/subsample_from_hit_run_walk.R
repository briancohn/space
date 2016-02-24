library(rgl)
subsample_every_nth_row <- function(dataframe, interval){
  dataframe[seq(interval, length(dataframe[,1]), interval),]
}

fulldata <- read.csv("output/toy_example_recursive1.78666N_positive1456345658817.csv",header=FALSE)
subsampled_data <- subsample_every_nth_row(fulldata, 100)
write.csv(subsampled_data, "output/cadaver_ready_data.csv", row.names=FALSE)

