library(rgl)
subsample_every_nth_row <- function(dataframe, interval){
  dataframe[seq(interval, length(dataframe[,1]), interval),]
}

feasible_x_for_null       <- read.csv("output/toy_example_recursive1.78666N_positive1456345658817.csv",header=FALSE)
feasible_x_for_palmar     <- read.csv("output/toy_example_recursive0.013339999999999907N_positive1456345537716.csv", header=FALSE)
feasible_x_for_off_palmar <- read.csv("output/toy_example_recursive5.3332999999999995N_positive1456345661011.csv", header=FALSE)

subsampled_null   <- subsample_every_nth_row(feasible_x_for_null, 100)
subsampled_palmar <- subsample_every_nth_row(feasible_x_for_palmar,100)
subsampled_off_palmar <- subsample_every_nth_row(feasible_x_for_off_palmar,100)

null_and_palmar <- merge(subsampled_null, subsampled_palmar)
subsampled_data <- merge(null_and_palmar, subsampled_off_palmar)


write.csv(subsampled_data, "output/cadaver_ready_data.csv", row.names=FALSE)

