fixed_alpha_muscle_histograms <- function (filename, fixedl_alpha_val){

	print('Computing Activation distribution for a fixed alpha=0.8, and force in the X direction.')

	datafolder_path = "~/Documents/dev/space/output/"
	db <- read.csv(paste0(datafolder_path, filename), header=FALSE )
	num_muscles <- 7
	m_names <- c("fp", "fs", "di","pi","ei","lum","ec", "fx", "fy" , "fz", "tx", "alpha", "l1", "l2", "l3", "l1w", "l2w", "l3w") #as in the input data from FVC dissertation
	colnames(db) <- m_names
	db <- db[c(2,1,5,7,6,3,4, 8,9,10,11,12,13,14,15,16,17,18)] #reorganize to 1998 FVC

	maskAlpha8<- abs(db['alpha']-fixedl_alpha_val) < 1E-6
	db <- db[maskAlpha8,]

	pdf('~/Documents/dev/space/src/latex/figs/raw_histograms.pdf')

	par(mfrow=c(4,2))
	for (i in 1:num_muscles) {
		breaks_vec = seq(0,1.0,length.out=50)
		hist(db[,i], xlim=c(0,1), main=colnames(db)[i], breaks=breaks_vec, xlab='Activation')
		# Plot the observed bounds
		abline(v=max(db[,i]),col="#A13F25")
		abline(v=min(db[,i]),col="#A13F25")
	}

	dev.off()
	browser()
}