cutoff_dotCSV<- function(filename){
	len <- nchar(filename)
	return(str_sub(filename, 0, len-4))
}

subsample_every_nth_row <- function(dataframe, interval){
  dataframe[seq(interval, length(dataframe[,1]), interval),]
}