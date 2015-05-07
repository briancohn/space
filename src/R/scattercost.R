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