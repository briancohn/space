solution_text <- function (musclename, val, x, y ){
	text(x = x, y = x, paste("a", "=", val),
     cex = 1.6, col = "black")
}
#there must be a column called "alpha" that is accessible via _$alpha
marchPlot <- function(
	filename, 
	NumMuscles = 7,
	colNames, ...
	)
{
	outputpath<- "output/"
	db <- read.csv(paste0(outputpath, filename), header=FALSE)
	colnames(db) <- colNames
	#for each alpha make a set of 'num_muscles' histograms
	alphavals <- sort(unique(db$alpha), decreasing=FALSE)
	total_num_alpha_steps <- length(alphavals)
	par(mfcol=c(total_num_alpha_steps,NumMuscles))
	par(mar=c(0.8,1,1,1))

	for (j in seq(1, NumMuscles)) {
		for (i in alphavals) {
			if(i == tail(alphavals, n=1)) {
				plotxaxt=TRUE
			} else {
				plotxaxt=FALSE
			}
			sample <- db[which(abs(db$alpha-i) <0.001),]
			print(paste('muscle',j, 'at alphaval', i ))
			print(summary(sample[,j]))
			d <- density(sample[,j])
			myHistogram <- hist(x=sample[,j],
				plot=FALSE,
				breaks=seq(0.0,1.0,length.out=50))
			myHistogram$counts <- myHistogram$counts*100.0/sum(myHistogram$counts)
			plot(myHistogram, ylim=c(0.0,max(myHistogram$counts)),
							 ylab='',
							 xlab='',
							 xaxt='n',
							 main=NULL,
							 col="#A13F25",
							 lty="blank",
							 tck=-0.01)
			soln_at_maximal <- mean(db[which(abs(db$alpha-1.0) <0.001),][,j])
			scaled_sln <- soln_at_maximal*i
			if(plotxaxt){
				axis(1, seq(0,1, length.out=5))
				# solution_text('pido', val=mean(sample[,j]), 0.5, 50 )
			} else if (i==0.1) {
				title(main=c("fdp", "fds", "eip", "edc", "lum", "di", "pi")[j])
				abline(v=scaled_sln, col="grey", lwd=1)
				abline(v=max(sample[,j]),col="#3F4878", lty=3)
				abline(v=min(sample[,j]),col="#3F4878", lty=3)
			} else	{		
				abline(v=scaled_sln, col="grey", lwd=1)
				abline(v=max(sample[,j]),col="#3F4878", lty=3)
				abline(v=min(sample[,j]),col="#3F4878", lty=3)
			}
			# lines(d, col="brown3", xlim= c(0.0,1.0))
			# //Plot observed bounds

			abline(a=0,b=0,col="#000000")
		}
	}
}
make_alpha_progression_pdfs <- function(csvlist, datafolder_path="", columnNames, ...){
	print('Computing Pages of Activation Progression Histograms')
	for (i in 1:length(csvlist)){
		#plot three separate alpha progressions- one page for each direction that is being marched along.
		pdf(paste0('src/manuscript/figs/', csvlist[i], '.pdf'), width=7.5, height=8.5 , pointsize=8)
			marchPlot(paste0(datafolder_path,csvlist[i], '.csv'), colNames = columnNames, ...)
		dev.off()
	}
}

activation_progressions<- function(csvlist, ...){
	make_alpha_progression_pdfs(csvlist, ...)
}
