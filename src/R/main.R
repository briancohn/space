library(stringr)
source('src/R/csv_helpers.R')
source('src/R/get_max_alpha_solutions.R')
# Make the toy histogram figure for one alpha



csvlist = c(
"Z_alphaProgression1430924065026.csv"
)
# columnNames <- c("fdp", "fds", "eip", "edc", "lum", "di", "pi", 
	# "fx", "fy" , "fz", "tx", "alpha", "l1", "l2", "l3", "l1w", "l2w", "l3w")

columnNames= c(
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
raw_histogram_i <- 1
alpha_col <- match("alpha", columnNames)
csvlist_cutoff<- cutoff_dotCSV(csvlist)


filename = csvlist[raw_histogram_i]
outputpath <- "output/"
fixed_db <- read.csv(paste0( outputpath, filename), header=FALSE )
num_muscles <- 7
colnames(fixed_db) <- columnNames

figPath = 'src/manuscript/figs/'
print(paste(csvlist[raw_histogram_i], "is being generated as raw_histograms.pdf"))
max_alpha_solutions <- get_max_alpha_solutions(fixed_db, num_muscles=num_muscles, alpha_col, header=FALSE)
source('src/R/fixed_alpha_muscle_histograms.R')
fixed_alpha_muscle_histograms(fixed_db,0.5, abline_vals=max_alpha_solutions, num_muscles=7, outPath=figPath)
source('src/R/activation_progressions.R')
activation_progressions(csvlist_cutoff[1], columnNames=columnNames, NumMuscles = num_muscles, outPath=figPath)

partial_fixed_db<- fixed_db
half <- partial_fixed_db[,'alpha']==0.5
colnames(partial_fixed_db) <- c("fdp", "fds", "eip", "edc", "lum", "di", "pi", "fx", "fy" , "fz", "tx", "alpha", "l1", "l2", "l3", "l1w", "l2w", "l3w")
write.csv(partial_fixed_db[half,], 
	file='src/parcoord/examples/data/Z_alphaProgression1430924065026only_fifty_percent.csv', 
	row.names=FALSE,
	quote=FALSE)

source('src/R/scattercost.R')
##########produce the cost-function correlations #######
#extract only where alpha = 50%
justhalf = fixed_db[fixed_db['alpha']==0.5,]
pdf("src/manuscript/figs/cost_function_scatterplots.pdf", width=12, height=4.8)
scatter_cost_unweighted(justhalf)
scatter_cost_weighted(justhalf)
dev.off()