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
	plot.new()
	par(mfcol=c(9,7))
	par(mar=c(1,1,1,1))
	for (j in seq(1, NumMuscles)) {
		for (i in seq(0.1,0.9,by=0.1)) {
			sample <- db[which(db$alpha-i <0.001),]
			YMAX <- 20000
			print(summary(sample[,j]))
			hist(sample[,j], xlab="Activation", ylab="Points", cex = 0.2, ylim = c(0.0,YMAX),col="darkgreen", main="", cex=0.1, xlim = c(0.0,1.0), breaks=10)	
			# //Plot observed bounds
			abline(v=max(db[,j]),col="purple")
			abline(v=min(db[,j]),col="purple")
		}
	}
}

datafolder_path = "~/Documents/dev/space/output/"
marchPlot(paste0(datafolder_path, "X_alphaProgression1426209965993.csv"))