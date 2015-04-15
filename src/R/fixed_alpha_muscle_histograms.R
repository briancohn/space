fixed_alpha_muscle_histograms <- function (filename, fixedl_alpha_val, columnNames, abline_vals=NULL) {

	print('Computing Activation distribution for a fixed alpha, and force in one given direction.')

	outputpath <- "output/"
	db <- read.csv(paste0( outputpath,filename), header=FALSE )
	num_muscles <- 7
	colnames(db) <- columnNames

	maskAlpha8 <- abs(db['alpha']-fixedl_alpha_val) < 1E-6
	db <- db[maskAlpha8,]
	pdf('src/latex/figs/raw_histograms.pdf')
	par(mfrow=c(4,2))
	for (i in 1:num_muscles) {
		breaks_vec = seq(0,1.0,length.out=50)
		hist(db[,i], xlim=c(0.0,1.0), main=colnames(db)[i], breaks=breaks_vec, xlab='Activation',
								freq=TRUE,
								bty="n",
								col="#A13F25",
								lty="blank"
									)
		# Plot the observed bounds
		abline(v=max(db[,i]),col="orange", lty=2)
		abline(v=min(db[,i]),col="orange", lty=2)
		if (class(abline_vals) != "NULL") {
			abline(v=abline_vals[i], col="darkgrey", lwd=2)
		}
	}

	dev.off()
}
