library(stringr)
source('fixed_alpha_muscle_histograms.R')
source('activation_progressions.R')


# @The third element in the list of csv's will be the X direction, and will be used for the fixed 0.8 histogram.
csvlist = c(
"XY_alphaProgression1426556273055.csv",
# "X_alphaProgression1426555010738.csv",
"100sampled_X_alphaProgression1426556261274.csv",
"Y_alphaProgression1426556267348.csv"
)

cutoff_dotCSV<- function(filename){
	len <- nchar(filename)
	return(str_sub(filename, 0, len-4))
}

csvlist_cutoff<- cutoff_dotCSV(csvlist)
print(paste(csvlist[3], "is being generated as raw_histograms.pdf"))
fixed_alpha_muscle_histograms(csvlist[3], 0.8)
activation_progressions(csvlist_cutoff)
