# takes in the csv filename, outputs the activation solutions where alpha=0
get_max_alpha_solutions <- function(csvFilename, num_muscles, whichColumnHasAlpha, ...) {
	points <- read.csv(csvFilename, ...)
	solutions <- c()
	for (col in 1:num_muscles) {
		solutions <- c(solutions, max(points[,col]))
	}
	return(solutions)
}