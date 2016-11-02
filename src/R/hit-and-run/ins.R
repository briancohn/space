# input variables
source("Parallel_Plot.R")
require(plyr)

# JR matrix (not needed)
data= c(-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669,
        -0.04689, -0.1496, 1.456*10^-17, 0.0248, 0.052, 0.0248, 0.052,
        0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518,
        0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
JR = matrix(data,4,7,byrow = TRUE)
# fmax is the maximal force output
fmax=c(123, 219, 124.8, 129.6, 23.52, 21.6, 91.74)

main <- function(path="~/Documents/GitHub/bcohn12/space/output/", output_directory="~/Documents/GitHub/bcohn12/space/src/R/hit-and-run/plots/") {
	# produce all image frame 
	files <- list.files(path, pattern="*.csv", full.names=T, recursive=FALSE)
	lapply(files, function(file) {
	  paracoord_plot(file, fmax, 9, 0.7, output_directory)
	})
}

main()

#afterwards, take the pictures in the folder and make a GIF or movie.