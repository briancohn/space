cutoff_dotCSV<- function(filename){
	len <- nchar(filename)
	return(str_sub(filename, 0, len-4))
}