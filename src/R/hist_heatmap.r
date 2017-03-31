source('vectormap.r')
source('distal_progression_csv_filename_list.r')

## @param point_matrix a matrix where each row is a n-dimensional point sampled. columns are muscles.
## @return val a list of histogramdatastructures
histogram_all_columns <- function(point_matrix, binwidth=0.02){
	iterator <- 1:ncol(point_matrix)

	lapply(
		iterator,
		function(i){
				hist(point_matrix[,i], breaks=seq(0,1,by=binwidth), plot=FALSE)
			}
	)
}

get_nth_element_from_all_sublists <- function(big_list, n) {
	lapply(big_list, function(sublist) {sublist[n]})
}

#by default, it gets only the length of the first sublist.
get_length_of_sublist <- function(big_list, n=1){
	return(ncol(big_list[[n]]))
}

get_nth_element_of_sublists <- function(list_of_sublists, n){
	lapply(list_of_sublists, 
		function(x) {
			x[[n]]
			})
}

get_matrix_of_counts <- function(list_of_histograms_for_a_given_muscle){
	res <- lapply(list_of_histograms_for_a_given_muscle, 
		function(x) {
			x$count
		})
	return(do.call(rbind,res)) #convert from list of vectors into matrix (index=>rownum)
}
library(fields)
plot_force_progression_map <- function(histogram_progression_matrix){
	#force 0 to be pure black, and next to zero to be bright orange.
	color_ramp_PuOr <- colorRampPalette(c("#f1a340","#f7f7f7","#998ec3"))(99)
	color_ramp <- c("#000000", color_ramp_PuOr) 
	fields::image.plot(histogram_progression_matrix, col= color_ramp, axes=FALSE,xlab="",ylab="")
	axes(histogram_progression_matrix,0,1)
}

get_unix_time_string_now <- function()
	{return(as.character(as.numeric(Sys.time())))}

density_normalization <- function(list_of_histogram_matrices_count) {
	sample_size = sum(list_of_histogram_matrices_count[1,])
	return(list_of_histogram_matrices_count/sample_size)
}

#main#
#if you want to use a different progression of more points, put the new filenames here
require(parallel)
# by default it's just looking in the current folder.
main <- function(filename_list, folder_path = ""){
	#specify the filenames of the data for analysis
	list_of_point_matrices <- lapply(
		filename_list,
		function(i){
	  		read.csv(paste0(folder_path,i), header=FALSE)
		})
	message("All point sets from CSV are in active Memory.")

	list_of_histogram_sublists <- lapply(list_of_point_matrices, histogram_all_columns, binwidth=0.05)
	
	#split the ublists and reorder into histograms by muscle
	# tasks_iterator = 1:length(finger_alpha_progression_filenames)
	#get the number of muscles and construct an iterator
	muscle_iterator = 1:get_length_of_sublist(list_of_point_matrices)
	message("All samples have been broken into histogram bins")
	list_of_histogram_progressions <- lapply(muscle_iterator, 
		function(x) {
				get_nth_element_of_sublists(list_of_histogram_sublists,x)
		}
	)
	#for each muscle, make an image
	message("4")
	list_of_histogram_matrices <- lapply(list_of_histogram_progressions, get_matrix_of_counts)
	list_of_histogram_matrices_density <- lapply(list_of_histogram_matrices, density_normalization)
	pdf(paste0(get_unix_time_string_now(), "quicktry2_histogram_heatmap.pdf"), height = 3.5, width = 21)
	par(mfrow=c(1,7))
	lapply(list_of_histogram_matrices_density, plot_force_progression_map)
	dev.off()
	message('plotting over')
}


# here we are calculating the percent of explained variance for each principal component. 
#To this plot, we will add a line that indicates the amount of variance each variable 
# would contribute if all contributed the same amount.
# https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/#ref2
pca_muscle_solution_space <- function(filename, folder_path){
	dataset <- read.csv(paste0(folder_path,filename), header=FALSE)
	# log transform 
	require(caret)
	number_of_muscles = length(dataset[1,])
	dataset.pca <- prcomp(dataset, retx=TRUE, center=TRUE, scale.=TRUE)
	sd <- dataset.pca$sdev
	loadings <- dataset.pca$rotation
	rownames(loadings) <- colnames(dataset)
	scores <- dataset.pca$x
	var <- sd^2
	var.percent <- var/sum(var) * 100
	dev.new()
	barplot(var.percent, xlab="PC", ylab="Percent Variance", main=filename, names.arg=1:length(var.percent), las=1, ylim=c(0,max(var.percent)), col="gray")
	abline(h=1/ncol(dataset)*100, col="red")
	print(loadings)
}




# main(finger_alpha_progression_filenames)

filenames_to_visualize <- distal_progression_csv_filename_list_len_10()
main(filenames_to_visualize, folder_path = '~/Documents/GitHub/bcohn12/space/output/n_1000_alphalen_10/')
# dev.off()
# plot.new()
# par(mfrow=c(1,3))
# pca_muscle_solution_space('finger2.881155463796023E-71474701464678.csv', '~/Documents/GitHub/bcohn12/space/output/')
# pca_muscle_solution_space('finger23.0492437103681881475092289825.csv', '~/Documents/GitHub/bcohn12/space/output/')
# pca_muscle_solution_space('finger28.8115546350790771475101516287.csv', '~/Documents/GitHub/bcohn12/space/output/')
# pca_muscle_solution_space('finger25.9078901456739781475102067373.csv', '~/Documents/GitHub/bcohn12/space/output/')
# main(long_list[seq(0,1000, length.out=20)], folder_path = '~/Documents/GitHub/bcohn12/space/output/')