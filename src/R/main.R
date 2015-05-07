library(stringr)
source('src/R/csv_helpers.R')
source('src/R/get_max_alpha_solutions.R')
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
	sep=",",
	quote=FALSE)



#@param db Matrix where entires are row-wise. labeled column names required. must have columns called 'l1','l2','l3'
scatter_cost_unweighted<- function(db) {
	costFnNames = c("l1", "l2", "l3", "l1w", "l2w", "l3w")
	par(mfrow=c(1,3))
	cost.lm <- lm(l1~l2, data=db)
	r2Val <- summary(cost.lm)$r.squared
	plot(db[,'l1'], db[,'l2'], xlab=costFnNames[1], ylab=costFnNames[2], main="", pch=20, asp=1,cex=0.2)
	title(main=paste("rsquared =",r2Val))
	cost.lm <- lm(l1~l3, data=db)
	r2Val <- summary(cost.lm)$r.squared
	plot(db[,'l1'], db[,'l3'], xlab=costFnNames[1], ylab=costFnNames[3], main="", pch=20, asp=1,cex=0.2)
	title(main=paste("rsquared =",r2Val))
	cost.lm <- lm(l2~l3, data=db)
	r2Val <- summary(cost.lm)$r.squared
	plot(db[,'l2'], db[,'l3'], xlab=costFnNames[2], ylab=costFnNames[3], main="", pch=20, asp=1,cex=0.2)
	title(main=paste("rsquared =",r2Val))
}

#@param db Matrix where entires are row-wise. labeled column names required. must have columns called 'l1w','l2w','l3w'
scatter_cost_weighted<- function(db) {
	costFnNames = c("l1", "l2", "l3", "l1w", "l2w", "l3w")
	par(mfrow=c(1,3))
	cost.lm <- lm(l1w~l2w, data=db)
	r2Val <- summary(cost.lm)$r.squared
	plot(db[,'l1w'], db[,'l2w'], xlab=costFnNames[4], ylab=costFnNames[5], main="", pch=20, asp=1,cex=0.2)
	title(main=paste("rsquared =",r2Val))
	cost.lm <- lm(l1w~l3w, data=db)
	r2Val <- summary(cost.lm)$r.squared
	plot(db[,'l1w'], db[,'l3w'], xlab=costFnNames[4], ylab=costFnNames[6], main="", pch=20, asp=1,cex=0.2)
	title(main=paste("rsquared =",r2Val))
	cost.lm <- lm(l2w~l3w, data=db)
	r2Val <- summary(cost.lm)$r.squared
	plot(db[,'l2w'], db[,'l3w'], xlab=costFnNames[5], ylab=costFnNames[6], main="", pch=20, asp=1,cex=0.2)
	title(main=paste("rsquared =",r2Val))
}
##########produce the cost-function correlations #######
#extract only where alpha = 50%
justhalf = fixed_db[fixed_db['alpha']==0.5,]
# Plot all combinations

pdf("cost_function_scatterplots.pdf", width=6, height=2.4)
scatter_cost_unweighted(justhalf)
scatter_cost_weighted(justhalf)
dev.off()