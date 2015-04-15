library(stringr)
source('src/R/fixed_alpha_muscle_histograms.R')
source('src/R/activation_progressions.R')
source('src/R/csv_helpers.R')


# @The third element in the list of csv's will be the X direction, and will be used for the fixed 0.8 histogram.
csvlist = c(
"X_alphaProgression1429065068687.csv",
"XY_alphaProgression1429065083969.csv",
"Y_alphaProgression1429065076877.csv",
"Z_alphaProgression1429065090943.csv" 
)
columnNames = c("fdp", "fds", "eip", "edc", "lum", "di", "pi", "fx", "fy" , "fz", "tx", "alpha", "l1", "l2", "l3", "l1w", "l2w", "l3w")

csvlist_cutoff<- cutoff_dotCSV(csvlist)
print(paste(csvlist[3], "is being generated as raw_histograms.pdf"))
max_alpha_solutions <- c(0.5,0.5,0.5,0.5,0.5,0.5,0.5)

fixed_alpha_muscle_histograms(csvlist[4], 1.0, columnNames, abline_vals=max_alpha_solutions)
activation_progressions(csvlist_cutoff, columnNames=columnNames)
