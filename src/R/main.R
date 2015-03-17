library(stringr)
source('fixed_alpha_muscle_histograms.R')
source('activation_progressions.R')


# @The third element in the list of csv's will be the X direction, and will be used for the fixed 0.8 histogram.
csvlist = c(
"XY_alphaProgression1426555016140.csv",
"X_alphaProgression1426555010738.csv",
"Y_alphaProgression1426555013471.csv"
)

cutoff_dotCSV<- function(filename){
	len <- nchar(filename)
	return(str_sub(filename, 0, len-4))
}

csvlist_cutoff<- cutoff_dotCSV(csvlist)
print(paste(csvlist[3], "is being generated as raw_histograms.pdf"))
fixed_alpha_muscle_histograms(csvlist[3], 0.8)
activation_progressions(csvlist_cutoff)