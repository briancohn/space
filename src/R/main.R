library(stringr)
source('src/R/fixed_alpha_muscle_histograms.R')
source('src/R/activation_progressions.R')
source('src/R/csv_helpers.R')
source('src/R/get_max_alpha_solutions.R')


# @The third element in the list of csv's will be the X direction, and will be used for the fixed 0.8 histogram.
csvlist = c(
"X_alphaProgression1429065877379.csv",  "XY_alphaProgression1429067471576.csv",
"Y_alphaProgression1429066661453.csv",  "Z_alphaProgression1429068338131.csv" 
)
# columnNames <- c("fdp", "fds", "eip", "edc", "lum", "di", "pi", 
	# "fx", "fy" , "fz", "tx", "alpha", "l1", "l2", "l3", "l1w", "l2w", "l3w")

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
raw_histogram_i <- 4
alpha_col <- match("alpha", columnNames)
csvlist_cutoff<- cutoff_dotCSV(csvlist)
print(paste(csvlist[raw_histogram_i], "is being generated as raw_histograms.pdf"))
max_alpha_solutions <- get_max_alpha_solutions(paste0('output/',csvlist[raw_histogram_i]), num_muscles=num_muscles, alpha_col, header=FALSE)
fixed_alpha_muscle_histograms(csvlist[raw_histogram_i], 0.5, columnNames, abline_vals=max_alpha_solutions)
activation_progressions(csvlist_cutoff, columnNames=columnNames, NumMuscles = num_muscles)
