# takes in the csv filename, outputs the activation solutions where alpha=1.0
get_max_alpha_solutions <- function(points, num_muscles, whichColumnHasAlpha, ...) {
	
	solutions <- c()
	maxpoints <- points[points[,whichColumnHasAlpha]==1.0,]
	for (col in 1:num_muscles) {
		solutions <- c(solutions, max(points[,col]))
	}
	return(solutions)
}