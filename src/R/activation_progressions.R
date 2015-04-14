marchPlot <- function(
	filename, 
	NumMuscles = 7,
	m_names, ...
	)
{
	outputpath<- "output/"
	db <- read.csv(paste0(outputpath, filename), header=FALSE)
	colnames(db) <- m_names
	#for each alpha make a set of 'num_muscles' histograms
	par(mfcol=c(9,7))
	par(mar=c(0.8,1,1,1))
	for (j in seq(1, NumMuscles)) {
		for (i in seq(0.1,0.9,by=0.1)) {
			sample <- db[which(abs(db$alpha-i) <0.001),]
			d <- density(sample[,j])
			hist(sample[,j], xlab="", ylab="",
				col="#A13F25", 
				main=paste(colnames(db)[j], "a=", i), 
				cex.axis=0.5, 
				xlim = c(0.0,1.0), 
				bty="n",
				lty="blank",
				freq=FALSE,
				breaks=seq(0,1,length.out=50))
			# lines(d, col="brown3", xlim= c(0.0,1.0))
			# //Plot observed bounds
			abline(v=max(sample[,j]),col="#3F4878")
			abline(v=min(sample[,j]),col="#3F4878")
		}
	}
}
make_alpha_progression_pdfs <- function(csvlist, datafolder_path="", columnNames){
	print('Computing Pages of Activation Progression Histograms')
	for (i in 1:length(csvlist)){
		#plot three separate alpha progressions- one page for each direction that is being marched along.
		pdf(paste0('src/latex/figs/', csvlist[i], '.pdf'), width=7.5, height=8.5 , pointsize=8)
			marchPlot(paste0(datafolder_path,csvlist[i], '.csv'), m_names = columnNames)
		dev.off()
	}
}

activation_progressions<- function(csvlist, ...){
	make_alpha_progression_pdfs(csvlist, ...)
}
