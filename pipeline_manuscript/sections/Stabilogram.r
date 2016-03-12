
#This is a testing function that loads in some example data and creates a graph
main <- function () {
	stabilogramData <- read.csv("input_data/advertising-and-sales-data-36-co.csv")
	#mainStabilogramPlot(stabilogramData[2]$Advertising, stabilogramData[3]$Sales, "Stabilogram", c("Settling Time = 36", "LyE = 0.3124125", "Area = 1253 cm^2"), 'blue')
	#simpleStabilogramPlot(stabilogramData[2]$Advertising, stabilogramData[3]$Sales,'blue')
	compoundSimpleStabilogramPlot(stabilogramData[2]$Advertising, stabilogramData[3]$Sales, stabilogramData[2]$Advertising, stabilogramData[3]$Sales,'red','blue', "figures/RightLeftLabel.png")
}

#This functions prints out a basic stabilogram, which is actually a scatterplot using the 
#data in two vectors, x and y, as its x and y coordinates.  It prints lines and hides the
#points so as to replicate the origin feel.  It prints the text in labelVector to the legend,
#(pass it an empty vector, c(), to omit the label),
#and it prints the convex hull around the points, using a function from online.

#Note that area, LyE and so on should actually be calculated beforehand if they
#are to be used, and passed to this function as text.
mainStabilogramPlot <- function(x, y, subtitle, labelVector, color) {
	#Create the plot with the given data, don't show the points, and set the axis labels and title.
	plot(x = x, y = y, type = "n", main=subtitle, xlab="COP (cm)", ylab="COP(cm)")
	
	#draw the lines -- basically, put a line segment between each adjacent pair of points.
	s <- seq(length(x)-1)  # one shorter than data
	s <- s[-length(s)]
	segments(x[s], y[s], x[s+1], y[s+1], col= color, lwd = 2)
	
	#plot the convex hull of the data on the stabilogram in red.
	Plot_ConvexHull(xcoord = x, ycoord = y, lcolor = "red")
	
	#find the upper-left corner, and put the legend there, if we have one.
	if(length(labelVector != 0)) {	
		usr <- par( "usr" )
		legend(usr[1], usr[4], labelVector )
	}
}



#This functions prints out a barebones stabilogram with only the lines, and no convex hull.
#Pass the requested color to the function.
#This can be used for the graphs in the "Explosive Power" section
simpleStabilogramPlot <- function(x, y, color) {
	#Create the plot with the given data, don't show the points, and set the axis labels and title.
	plot(x = x, y = y, type = "n", axes = FALSE, ylab = "", xlab = "")
	
	#draw the lines -- basically, put a line segment between each adjacent pair of points.
	s <- seq(length(x)-1)  # one shorter than data
	s <- s[-length(s)]
	segments(x[s], y[s], x[s+1], y[s+1], col= color, lwd = 2)
	
	#Draw a box around the plot
	box(lty = 'solid', col = 'black')
}

#This functions creates the compound graph as seen in Explosive Power.  It prints two plots in a row,
#and then adds an image to the right side of the plots.
#Pass the requested data for the left and right plot to the function, along with the imagepath and the colors.
compoundSimpleStabilogramPlot <- function (x1, y1, x2, y2, color1, color2, imagepath) {
	#need to be able to import PNGs
	library(png)
	
	#Replace the directory and file information with your info
	displayImage <- readPNG(imagepath)

	#Set up the plot area -- three columns
	par(mfrow=c(1,3))

	#plot the two stabilograms
	simpleStabilogramPlot(x1, y1, color1)
	simpleStabilogramPlot(x2, y2, color2)
	
	#move to the next space
	plot.new()

	#save where the plot should go
	lim <- par()
	#draw the image in that space
	rasterImage(displayImage, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
	
}

### Plotting function to plot convex hulls
### Notes: Stolen from the internet.
############################################################################

# INPUTS:
# xcoords: x-coordinates of point data
# ycoords: y-coordinates of point data
# lcolor: line color

# OUTPUTS:
# convex hull around data points in a particular color (specified by lcolor)

# FUNCTION:
Plot_ConvexHull<-function(xcoord, ycoord, lcolor){
  hpts <- chull(x = xcoord, y = ycoord)
  hpts <- c(hpts, hpts[1])
  lines(xcoord[hpts], ycoord[hpts], col = lcolor, lwd = 2)
}  
# END OF FUNCTION