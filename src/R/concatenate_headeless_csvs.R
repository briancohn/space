
datafolder_path = "~/Documents/space/output"
setwd(datafolder_path)
filenames <- list.files(path = datafolder_path)
db = do.call("rbind", lapply(filenames, read.csv, header = FALSE))
m_names <- c("fp", "fs", "di","pi","ei","lum","ec")
colnames(db) <- m_names
# FP, FS, EI, EC, Lum, DI, PI
db <- db[c(2,1,5,7,6,3,4)] #reorganize


library(rgl)
Uppers <- c(0.31791515939146464, 0.14050552684500817, 0.5742125648636693, 0.44270880966177634, 1.0, 1.0, 1.0)
Lowers <- c(0.019536449497745724, 0.0, 0.0, 0.0053061443199450865, 0.0, 0.0, 0.0)

pdf('raw_histograms.pdf')

par(mfrow=c(4,2))
for (i in 1:length(db[1,])) {
	hist(db[,i], xlim=c(0,1))
	# Plot the observed bounds
	abline(v=max(db[,i]),col="purple")
	abline(v=min(db[,i]),col="purple")
	# Plot the actual bounds
	# abline(v=Uppers[i],col="black")
	# abline(v=Lowers[i],col="black")

}


dev.off()
pdf('raw_barplots.pdf')

boxplot(
	db[,1],
	db[,2],
	db[,3],
	db[,4],
	db[,5],
	db[,6],
	db[,7] )
dev.off()

sample <- db[db['ei']>0.5,]
boxplot(sample[,1],sample[,2],sample[,3],sample[,4],sample[,5],sample[,6],sample[,7])


# plot3d(db[,1],db[,2],db[,3])
