library(stringr)
source('src/R/fixed_alpha_muscle_histograms.R')
source('src/R/activation_progressions.R')
source('src/R/csv_helpers.R')


# @The third element in the list of csv's will be the X direction, and will be used for the fixed 0.8 histogram.
csvlist = c(
"XY_alphaProgression1428961500565.csv",
"Y_alphaProgression1428961490683.csv",
"X_alphaProgression1428961481545.csv"
)
columnNames = c("fdp", "fds", "eip", "edc", "lum", "di", "pi", "fx", "fy" , "fz", "tx", "alpha", "l1", "l2", "l3", "l1w", "l2w", "l3w")

csvlist_cutoff<- cutoff_dotCSV(csvlist)
print(paste(csvlist[3], "is being generated as raw_histograms.pdf"))
fixed_alpha_muscle_histograms(csvlist[3], 0.8, columnNames)
activation_progressions(csvlist_cutoff, columnNames=columnNames)
