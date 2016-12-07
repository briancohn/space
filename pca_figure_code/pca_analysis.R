library(ggplot2)
library(devtools)

pca_loadings_and_component_info <- function(hitrun_point_dataframe) {
	res <- prcomp(hitrun_point_dataframe, scale=TRUE, center=TRUE)
	return(res)
}

add_finger_muscle_name_cols<- function(hitrun_point_dataframe){
	colnames(hitrun_point_dataframe) <- c("FDP","FDS","EIP","EDC","LUM","DI","PI")
	return(hitrun_point_dataframe)
}
#a is a dataframe containing hit and run points about the same task. (assumes cols are named #FDP
#FDS
#EIP
#EDC
#LUM
#DI
#PI)
center_all_point_about_col_medians <- function(a){
	a$FDP <- a$FDP-median(a$FDP)
	a$FDS <- a$FDS-median(a$FDS)
	a$EIP <- a$EIP-median(a$EIP)
	a$EDC <- a$EDC-median(a$EDC)
	a$LUM <- a$LUM-median(a$LUM)
	a$DI <- a$DI-median(a$DI)
	a$PI <- a$PI-median(a$PI)
	return(a)
}




main <- function(){
	csv_files <- csv_filename_list()
	list_of_hitrun_points <- lapply(csv_files, read.csv, header=FALSE)
	list_of_hitrun_points <- lapply(list_of_hitrun_points, add_finger_muscle_name_cols)
	browser()


	par(mfrow=c(2,2))
	pdf("progression_histograms.pdf")
		boxplot_hitrun_point(list_of_hitrun_points[[1]], "0 Newtons")
		boxplot_hitrun_point(list_of_hitrun_points[[4]], "9 Newtons")
		boxplot_hitrun_point(list_of_hitrun_points[[8]], "19 Newtons")
		boxplot_hitrun_point(list_of_hitrun_points[[9]], "25 Newtons")
	dev.off()


	list_of_hitrun_points_of_interest <- list(
		list_of_hitrun_points[[1]],
		list_of_hitrun_points[[2]],
		list_of_hitrun_points[[3]],
		list_of_hitrun_points[[4]],
		list_of_hitrun_points[[5]],
		list_of_hitrun_points[[6]],
		list_of_hitrun_points[[7]],
		list_of_hitrun_points[[8]],
		list_of_hitrun_points[[9]],
		list_of_hitrun_points[[10]]
	)
	list_of_pca_results <- lapply(list_of_hitrun_points_of_interest, pca_loadings_and_component_info)
	list_of_PC_importance_vectors <- lapply(list_of_pca_results, proportions_of_variance_explained)
	lineplot_success <- lapply(list_of_pca_results, simple_pca_line_plot)
	median_centered_example <- center_all_point_about_col_medians(list_of_hitrun_points[[1]])
	write.csv(median_centered_example, file = "median_centered_example_datapoints_force_at_zero.csv")
	return(0)
}

pc_importance_as_force_changes <- function(list_of_PC_importance_vectors) {
	res <- data.frame()
	for (i in 1:length(list_of_PC_importance_vectors)) {
		res[i,] <- list_of_PC_importance_vectors[[i]]
	}
}

simple_pca_line_plot <- function(prcomp_pca_result) {
	return(plot(prcomp_pca_result, type='l'))
}

#returns N variance levels from 0 to 1, in decreasing order.
# A high value means that the PC has high explaining capability
proportions_of_variance_explained <- function(prcomp_pca_result) {
	return(summary(prcomp_pca_result)[[6]][2,])
}


boxplot_hitrun_point<- function(hitrun_point_dataframe, force){
		boxplot(
			hitrun_point_dataframe,
			main=paste0("Task is ",force, " in X"),
			xlab="Muscle Number",
			ylab="Activation of the muscle"
	)
}
#manually list (in order of ascending forces) a progression of points. force.
csv_filename_list <- function(){
	c(
		"force_progression_10k_points/finger_forcevector_0.0_1479548844925.csv",
		"force_progression_10k_points/finger_forcevector_3.201283848342117_1479580661755.csv",
		"force_progression_10k_points/finger_forcevector_6.402567696684234_1479612494224.csv",
		"force_progression_10k_points/finger_forcevector_9.603851545026352_1479644460821.csv",
		"force_progression_10k_points/finger_forcevector_12.805135393368468_1479676465477.csv",
		"force_progression_10k_points/finger_forcevector_16.006419241710585_1479708480008.csv",
		"force_progression_10k_points/finger_forcevector_19.207703090052703_1479740485594.csv",
		"force_progression_10k_points/finger_forcevector_22.40898693839482_1479772993164.csv",
		"force_progression_10k_points/finger_forcevector_25.610270786736937_1479806592034.csv",
		"force_progression_10k_points/finger_forcevector_28.811554635079055_1479839473159.csv"
	)
}


main()

