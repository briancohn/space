# input variables
source("Parallel_Plot.R")
require(plyr)
require(tools)
require(stringr)

# JR matrix (not needed)
data= c(-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669,
        -0.04689, -0.1496, 1.456*10^-17, 0.0248, 0.052, 0.0248, 0.052,
        0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518,
        0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
JR = matrix(data,4,7,byrow = TRUE)
# fmax is the maximal force output
fmax=c(123, 219, 124.8, 129.6, 23.52, 21.6, 91.74)


# example input: "/Users/olive/Documents/GitHub/bcohn12/space/output/finger_26.306202058115677_1478105706655.csv"
# example output: 26.306202058115677 [as a numeric]

extract_float_from_filepath <- function(filepath) {
	return(extract_float(basename(filepath)))
}

#takes in a filename, and extracts a string of the float value
# example input: "finger_25.824403485622717_1478105706611.csv"
# output: "25.824403485622717"
extract_float <- function(filename) {
	# remove stuff thats not the float
  return(filename %>% str_match("[0-9\\.]+") %>% as.numeric)
}


sort_files_by_ascending_force_value <- function(list_of_filenames){
	force_vals_per_filename <- lapply(list_of_filenames, extract_float_from_filepath)
	#browser()
	indices = sort(unlist(force_vals_per_filename), index.return = TRUE) #get the indices from here, those will be your sorted indices
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/sort.html
	list_of_filenames[indices[[2]]]
}

main <- function(path="~/Documents/GitHub/bcohn12/space/output/", output_directory="~/Documents/GitHub/bcohn12/space/src/R/hit-and-run/plots/") {
	# produce all image frame 
	myfiles <- list.files(path, pattern="*.csv", full.names=T, recursive=FALSE)
	#browser()
	sorted_files <- sort_files_by_ascending_force_value(myfiles)
	lapply(sorted_files, function(file) {
	  paracoord_plot(file, fmax, 9, 0.8, outputdir=output_directory, outputname=extract_float_from_filepath(file))
	})
}

main(path = "~/dev/github/space/output/", output_directory = "~/dev/github/space/src/R/hit-and-run/plots/")

#afterwards, take the pictures in the folder and make a GIF or movie.