marchPlot <- function(
	filename, 
	NumMuscles = 7,
	m_names= c("fp", "fs", "di","pi","ei","lum","ec", "x", "y", "z", "tx", "alpha")
	)
{
	db <- read.csv(filename, header=FALSE)
	colnames(db) <- m_names
	# FP, FS, EI, EC, Lum, DI, PI
	db <- db[c(2,1,5,7,6,3,4,8,9,10,11,12)] #reorganize
	#for each alpha make a set of 'num_muscles' histograms
	par(mfcol=c(9,7))
	par(mar=c(0.8,1,1,1))
	for (j in seq(1, NumMuscles)) {
		for (i in seq(0.1,0.9,by=0.1)) {
			sample <- db[which(abs(db$alpha-i) <0.001),]
			d <- density(sample[,j])
			hist(sample[,j], xlab="", ylab="",
				col="aliceblue", 
				main="", 
				cex=0.05, 
				xlim = c(0.0,1.0), 
				freq=FALSE)
			lines(d, col="brown3", xlim= c(0.0,1.0))
			# //Plot observed bounds
			abline(v=max(db[,j]),col="purple")
			abline(v=min(db[,j]),col="purple")
		}
	}
}

datafolder_path = "~/Documents/dev/space/output/"
pdf('~/Documents/dev/space/src/latex/figs/mypdf.pdf')
	marchPlot(paste0(datafolder_path, "XY_alphaProgression1426215248933.csv"))
	marchPlot(paste0(datafolder_path, "Y_alphaProgression1426215236083.csv"))
	marchPlot(paste0(datafolder_path, "X_alphaProgression1426215214528.csv"))
dev.off()