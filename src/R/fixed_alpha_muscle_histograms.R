# Computing Activation distribution for a fixed alpha,
# and force in one given direction.
# outPath should have a trailing slash
# takes in the database, outputs n (n_muscles) histograms of projected point densities in histograms.
fixed_alpha_muscle_histograms <- function (  db,
											 fixedl_alpha_val,
											 abline_vals=NULL,
											 num_muscles,
											 outPath) {
	#extract only the values that are define in fixedl_alpha_val
	maskAlpha8 <- abs(db['alpha']-fixedl_alpha_val) < 1E-6
	db <- db[maskAlpha8,]

	#initialize the pdf for output
	pdf(paste0(outPath, 'raw_histograms.pdf'), width=3.1, height=9)
		par(mfrow=c(7,1),
			mar=c(1, 1.5, 1, 0),
			mgp=c(0, 0.25, -0.25),
			las=0
			)
		for (i in 1:num_muscles) {
			breaks_vec = seq(0,1.0,length.out=100)
			myHistogram <- hist(db[,i], 
									 
									# main=colnames(db)[i], 
									# xlab='Activation',
									# font.main = 1,
									breaks=breaks_vec, 
									ann=FALSE,
									tck=0.02,
									plot=FALSE
										)
			#convert to percentage (get fraction, multiply by 100)
			myHistogram$counts <- myHistogram$counts*100.0/sum(myHistogram$counts)
			plot(myHistogram, ylim=c(0.0,max(myHistogram$counts)),
							ylab='',
							 xlab="a",
							 main=NULL,
							 col="#A13F25",
							 lty="blank",
							 tck=-0.01)
			# Plot the observed bounds
			if (class(abline_vals) != "NULL") {
				abline(v=abline_vals[i], col="darkgrey", lwd=2)
			}
			abline(v=max(db[,i]), col="darkorange",lwd=1.5, lty=2)
			abline(v=min(db[,i]), col="darkorange",lwd=1.5, lty=2)
		}
	dev.off()
}
