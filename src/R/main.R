library(stringr)
source('src/R/fixed_alpha_muscle_histograms.R')
source('src/R/activation_progressions.R')
source('src/R/csv_helpers.R')
source('src/R/get_max_alpha_solutions.R')


# @The third element in the list of csv's will be the X direction, and will be used for the fixed 0.8 histogram.
csvlist = c(
"X_alphaProgression1429065068687.csv",
"XY_alphaProgression1429065083969.csv",
"Y_alphaProgression1429065076877.csv",
"Z_alphaProgression1429065090943.csv" 
)
# columnNames <- c("fdp", "fds", "eip", "edc", "lum", "di", "pi", "fx", "fy" , "fz", "tx", "alpha", "l1", "l2", "l3", "l1w", "l2w", "l3w")

columnNames <- c(
	"flexor digitorum profundus",
	"flexor digitorum superficialis",
	"extensor indicis proprius",
	"extensor digitorum communis",
	"lumbrical",
	"dorsal interosseous",
	"palmar interosseous",
	"fx",
	"fy",
	"fz",
	"tx",
	"alpha",
	"l1",
	"l2",
	"l3",
	"l1w",
	"l2w",
	"l3w"
  )
num_muscles <- 7
alpha_col <- match("alpha", columnNames)
csvlist_cutoff<- cutoff_dotCSV(csvlist)
print(paste(csvlist[3], "is being generated as raw_histograms.pdf"))
max_alpha_solutions <- get_max_alpha_solutions(paste0('output/',csvlist[4]), num_muscles=num_muscles, alpha_col)
fixed_alpha_muscle_histograms(csvlist[4], 0.5, columnNames, abline_vals=max_alpha_solutions)
activation_progressions(csvlist_cutoff, columnNames=columnNames)
