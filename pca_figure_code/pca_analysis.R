library(ggplot2)
library(devtools)
library(dplyr)
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

#you have to specify which PC you want
#@param n is the number of samples form the hit run dataframe
#@param PC is an integer, representing the PC you want. e.g. PC1 would be PC = 1
#@return pc_variance_explained a numeric \in [0,1]
variance_explained_for_a_PC_from_df <- function(hitrun_dataframe,n, PC){
	sample_hitrun_df <- sample_n(hitrun_dataframe,n)
	pca_info = pca_loadings_and_component_info(sample_hitrun_df)
	pc_variance_explained <- proportions_of_variance_explained(pca_info)[[PC]]
	return(pc_variance_explained)
}
get_vector_of_PC_variance_explained_for_subsampled_df <- function(hitrun_dataframe, sample_n, PC_of_interest, num_replicates){
	do.call(c, lapply(1:100, function(x) variance_explained_for_a_PC_from_df(hitrun_dataframe, sample_n, PC_of_interest)))
}

#the outliers threshold: outliers are: outside 1.5 times the interquartile range
variance_explained_boxplots_over_forceprogression <- function(list_of_hitrun_dataframes, sample_n, PC_of_interest, num_replicates){
	list_of_PC_variance_vectors <- lapply(list_of_hitrun_dataframes, function(x) get_vector_of_PC_variance_explained_for_subsampled_df(x, sample_n, PC_of_interest, num_replicates))
	names(list_of_PC_variance_vectors) <- c(0:9)
	op <- par(mar = rep(1, 4))
    boxplot(list_of_PC_variance_vectors, ylim=c(0,1), cex = 0.25, xlab="", ylab="", main="", asp=1.0, boxwex=0.4, whisklty=1, outpch=20)
    par(op)
}

main <- function(){
	list_of_hitrun_points <- lapply(csv_filename_list(), read.csv, header=FALSE)
	list_of_hitrun_dataframes <- lapply(list_of_hitrun_points, add_finger_muscle_name_cols)
	list_of_hitrun_points <- list_of_hitrun_dataframes

	pdf("pc1_progression.pdf", width= 16, height = 9.5, useDingbats=FALSE)
	par(mfrow=c(2,3))
	sample_sizes_to_evaluate_PC_on = c(10,100,1000)
	#do pc1
	lapply( sample_sizes_to_evaluate_PC_on,
		function(x) variance_explained_boxplots_over_forceprogression(list_of_hitrun_dataframes, sample_n=x, PC_of_interest=1, num_replicates=100)
		)
	#do pc2
	lapply( sample_sizes_to_evaluate_PC_on,
		function(x) variance_explained_boxplots_over_forceprogression(list_of_hitrun_dataframes, sample_n=x, PC_of_interest=2, num_replicates=100)
		)
	#do pc3
	# lapply( sample_sizes_to_evaluate_PC_on,
	# 	function(x) variance_explained_boxplots_over_forceprogression(list_of_hitrun_dataframes, sample_n=x, PC_of_interest=3, num_replicates=100)
	# 	)
	dev.off()
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

