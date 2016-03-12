main <- function () {
  if (!exists("SkirtData")) {
    SkirtData = read.csv("input_data/annual-diameter-of-skirt-at-hem-.csv")
  }
  SkirtData$randomMultiplier <- sample(-0.2:0.2, nrow(SkirtData), replace=TRUE)
  SkirtData$randomDiameters <- SkirtData$Annual.diameter.of.skirt.at.hem..1866.to.1911 * SkirtData$randomMultiplier
  simpleLineGraph(SkirtData$Year, 5, SkirtData$Annual.diameter.of.skirt.at.hem..1866.to.1911, 50, "Average Skirt Diamater, 1866-1911", "Year", "Average Skirt Diameter", SkirtData$randomDiameters, T,T,0,0,T)
}

simpleLineGraph <- function(time, xInterval, data1, yInterval, title, xlab, ylab, data2, shadedRange=F, maxLine=F, shadedDomainX0=0, shadedDomainX1=0, labelMax=F ) {
  # lwd option controls line thickness
  # cex.lab scales label size
  # cex.axis scales axis size
  plot(time, data1, type="l", xlab=xlab, ylab=ylab, col="blue", 
       lwd=2, axes=F, ann=T, cex.lab=0.8, cex.axis=0.8 )
  title(title)
  box(bty='L', lwd=2)
  axis(1, lab=T, tck=.02, lwd=2, at=seq(from=min(time),to=max(time),by=xInterval))
  axis(2, lab=T, tck=.02, lwd=2, at=seq(from=min(data1),to=max(data1),by=(max(data1)-min(data1))%%yInterval))
  
  
  #
  #NEXT COMMENTED OUT LINE WOULD PLOT SECOND RANDOMIZED DATA VECTOR IN SAME GRAPH
  #line(time, data2, lwd=1, col="red")
  #
  
  
  #Places dotted vertical line at abs. maximum when argument maxLine is true
  if (maxLine == T) {
    abline(v=approx(data1, time, max(data1)), lty=2)
  }
  
  if (labelMax == T) {
  #  locationX <- approx(data1, time, max(data1))
  #  locationY <- approx(time, data1, locationX)
  #  points(locationX, locationY)
  }
  
  #Places shaded region around range
  if (shadedRange == T) {
    x1 <- c(0, 2000, 2000, 0)
    y1 <- c((max(data1)+max(data1)*.005), (max(data1)+max(data1)*.005), (min(data1)-min(data1)*.005), (min(data1)-min(data1)*.005))
    polygon(x1, y1, col=rgb(.211,.211,.211,0.4), border=NA)
  }
  
  #Placed shaded region around domain specifed
    x2 <- c(shadedDomainX0, shadedDomainX0, shadedDomainX1, shadedDomainX1)
    y2 <- c(0, 1500, 1500, 0)
    polygon(x2, y2, col=rgb(.211,.211,.211,0.4), border=NA)
}
main()
