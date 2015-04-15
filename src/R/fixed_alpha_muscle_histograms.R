fixed_alpha_muscle_histograms <- function (filename, fixedl_alpha_val, columnNames, abline_vals=NULL) {

	print('Computing Activation distribution for a fixed alpha, and force in one given direction.')

	outputpath <- "output/"
	db <- read.csv(paste0( outputpath,filename), header=FALSE )
	num_muscles <- 7
	colnames(db) <- columnNames
	maskAlpha8 <- abs(db['alpha']-fixedl_alpha_val) < 1E-6
	db <- db[maskAlpha8,]
	pdf('src/latex/figs/raw_histograms.pdf', width=3.1, height=9)
	par(mfrow=c(7,1),
		mar=c(1, 1, 1, 1),
		mgp=c(0, 0.25, 0),
		las=0
		)
	for (i in 1:num_muscles) {
		breaks_vec = seq(0,1.0,length.out=100)
		hist(db[,i], xlim=c(0.0,1.0), 
								# main=colnames(db)[i], 
								breaks=breaks_vec, 
								# xlab='Activation',
								freq=TRUE,
								bty="n",
								# font.main = 1,
								col="#A13F25",
								lty="blank",
								########
								ann=FALSE,
								cex.lab=0.5,
								tck=0.02
									)
		# Plot the observed bounds
		if (class(abline_vals) != "NULL") {
			abline(v=abline_vals[i], col="darkgrey", lwd=2)
		}
		abline(v=max(db[,i]),col="darkorange",lwd=1.5, lty=2)
		abline(v=min(db[,i]),col="darkorange",lwd=1.5, lty=2)
	}

	dev.off()
}
