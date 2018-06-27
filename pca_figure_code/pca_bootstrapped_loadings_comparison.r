source('pca_analysis.R')
# tasks
# cyl to muscle number (factor of len7)
# am to force level (factor of len3)
# mpg to normalized PC1 muscle loading value

##' @param bootstrap_n_sizes a vector of integers representing the bootstrap lengths
generate_dataframe_of_varying_pca_bootstrap_size <- function(bootstrap_n_sizes, full_dataset_dataframe, PC_of_interest) {
	list_of_dataframes_of_bootstrap_experiments <- lapply(bootstrap_n_sizes, 
		function(x) produce_bootstrap_pca_experiment(n=x, full_dataset_dataframe, PC_of_interest)
		)
	return(do.call(cbind, list_of_dataframes_of_bootstrap_experiments))

}




rm_legend <- function(){
	theme(legend.position="none")
}
main <- function() {
	set.seed(100) #done for the purpose of scientific replicability =)
	#where each dataframe has many rows, each of which represents a PC loading vector.
	# list_of_PC_loadings_dataframes <- produce_bootstrap_pca_experiment(n=10, num_replicates=100, PC_of_interest=1)
	num_replicates=1000
	num_samples = 1000
	PC_of_interest=1

	#each level is a measure of force at the end of the finger
	message(1)
	list_of_hitrun_dataframes_for_different_forces <- list_10k_dataset_hitrun_dataframes()[c(1,2,3,4,5,6,7,8,9)]
	melted_loading_data <- pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, num_replicates,num_samples,PC_of_interest, snap_vector_signs_to_reference=TRUE)
	p <- loading_bootstrap_figure(melted_loading_data)
	plot_loadings_for_each_muscle_across_force_levels(melted_loading_data)


	pc1_1 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,10,1, snap_vector_signs_to_reference=TRUE)) + ylab("PC1") + rm_legend()
	pc1_2 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,100,1, snap_vector_signs_to_reference=TRUE)) + ylab("") + rm_legend()
	pc1_3 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,1000,1, snap_vector_signs_to_reference=TRUE))+ ylab("") + rm_legend()
	pc1_4 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,5000,1, snap_vector_signs_to_reference=TRUE), TRUE)+ ylab("") + rm_legend()
	
	pc2_1 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,10,2, snap_vector_signs_to_reference=TRUE))+ ylab("PC2") + rm_legend()
	pc2_2 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,100,2, snap_vector_signs_to_reference=TRUE), TRUE)+ ylab("") + rm_legend()
	pc2_3 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,1000,2, snap_vector_signs_to_reference=TRUE))+ ylab("") + rm_legend()
	pc2_4 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,5000,2, snap_vector_signs_to_reference=TRUE), TRUE)+ ylab("") + rm_legend()
	
	pc3_1 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,10,3, snap_vector_signs_to_reference=TRUE)) + ylab("PC3") + rm_legend()
	pc3_2 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,100,3, snap_vector_signs_to_reference=TRUE), TRUE) + ylab("") + rm_legend()
	pc3_3 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,1000,3, snap_vector_signs_to_reference=TRUE)) + ylab("") + rm_legend()
	pc3_4 <- loading_bootstrap_figure(pca_bootstrap_normalized_loadings_melted(list_of_hitrun_dataframes_for_different_forces, 1000,5000,3, snap_vector_signs_to_reference=TRUE)) + ylab("") + rm_legend()
	require(gridExtra)
	combined_figure <- grid.arrange(pc1_1,
									pc1_2,
									pc1_3,
									pc1_4,

									pc2_1,
									pc2_2,
									pc2_3,
									pc2_4,

									pc3_1,
									pc3_2,
									pc3_3,
									pc3_4,
									ncol=4)
	ggsave('pca_loadings_bootstrapped.png', combined_figure, width=30, height = 13, units="in")
}

plot_loadings_for_each_muscle_across_force_levels <- function(melted_loading_data){
	group_normalized_loadings_by_muscle <- lapply(finger_muscle_names_in_order(), function(muscle_name){
		extract_muscle_from_melted_loadings_data(melted_loading_data, muscle_name)
	})
	par(mfrow=c(1,7))
	lapply(group_normalized_loadings_by_muscle, plot_normalized_loading_over_force_for_a_muscle)
	par(mfrow=c(1,1))
}

pca_bootstrap_normalized_loadings_melted <- function(list_of_hitrun_dataframes_for_different_forces,
													 num_replicates,
													 num_samples,
													 PC_of_interest,
													 snap_vector_signs_to_reference=FALSE)
	{
	list_of_list_of_bootstrap_dataframes <- lapply(list_of_hitrun_dataframes_for_different_forces,
												produce_smaller_subsample_dataframes,
												num_replicates,
												num_samples
												)
	message(3)
	list_of_loadings_dataframes <- pbmclapply(list_of_list_of_bootstrap_dataframes, compute_many_replicates_of_loadings, PC_of_interest, mc.cores=8)

	if (snap_vector_signs_to_reference){
		# the second one appears to put the flexors both as positive. 
		# we will use this as our reference for sign.
		reference_vector <- list_of_loadings_dataframes[[1]][2,]
		list_of_loadings_dataframes <- lapply(list_of_loadings_dataframes,
			function(x) {
				return(assimilate_vector_signs_to_reference(x, reference_vector))
				}
		)
	}
	message(4)
	melted_loading_data <- melt_loadings_dataframes(list_of_loadings_dataframes)
	return(melted_loading_data)
}

extract_muscle_from_melted_loadings_data <- function(melted_loading_data, muscle_name){
	return(melted_loading_data[melted_loading_data$muscle_name==muscle_name,])
}

plot_normalized_loading_over_force_for_a_muscle <- function(loading_data){
	plot(loading_data$index_in_force_list, loading_data$loading_value, xlab="Index In Force List", ylab="Loading Value (normalized) ")
}

loading_bootstrap_figure <- function(melted_loading_data, FLIP_X=FALSE) {

	if (FLIP_X) {
		melted_loading_data$loading_value <- melted_loading_data$loading_value * -1.0
	}

	loading_bootstrapping_figure <- ggplot(melted_loading_data, aes(factor(muscle_name, levels=c("FDP", "FDS", "EIP", "EDC", "LUM", "DI", "PI")), loading_value)) + 
		geom_boxplot(aes(fill = factor(index_in_force_list))) +
		ylim(-1.0,1.0) +
		xlab("Muscle") +
		ylab("Normalized Loading Value from PCA")+
		theme_bw()+
		scale_color_manual(labels = c("low", "med", "high"), values = c("black", "blue", "lightblue"))

	return(loading_bootstrapping_figure)
}

melt_loadings_dataframes <- function(list_of_loadings_dataframes) {
	require(reshape)
	melted_loading_data <- melt(list_of_loadings_dataframes)
	colnames(melted_loading_data) <- c("replicate_number", "muscle_name", "loading_value", "index_in_force_list")
	melted_loading_data$index_in_force_list <- as.factor(melted_loading_data$index_in_force_list)
	return(melted_loading_data)
}

compute_many_replicates_of_loadings <- function(bootstrap_hitrun_dataframes, PC_of_interest){
	list_of_loadings_for_each_replicate <- lapply(bootstrap_hitrun_dataframes, get_loadings_for_PC, PC=PC_of_interest, normalize_to_max_abs_value=TRUE)
	loadings_dataframe <- bind_loadings_replicates_into_dataframe(list_of_loadings_for_each_replicate)
	return(loadings_dataframe)
}


##' @description each row is a different replicate
bind_loadings_replicates_into_dataframe <- function(list_of_loadings_for_each_replicate) {
	loadings_dataframe <- do.call(rbind,list_of_loadings_for_each_replicate) 
	rownames(loadings_dataframe) <- NULL #rownames are ancilliary
	return(loadings_dataframe)
}

produce_smaller_subsample_dataframes <- function(hitrun_dataframe, num_replicates, num_samples){
	list_of_list_of_sampling_dataframes <- lapply(1:num_replicates, function(x) {
		subsample_rows_from_dataframe(hitrun_dataframe, num_samples)
	})
	return(list_of_list_of_sampling_dataframes)
}

subsample_rows_from_dataframe <- function(df, n_sample_size){
	return(df[sample(nrow(df), n_sample_size), ])
}

##'@param n the number of hit and run samples to be collected for PCA.
##'@param full_dataset_dataframe the full dataframe from which the samples will be subsampled.
produce_bootstrap_pca_experiment <- function(n, num_replicates, PC_of_interest) {
	list_of_pc_loadings <- subsampled_PC_loadings_from_10k_dataset(n, PC_of_interest)
	list_of_many_PC_loading_vectors<- lapply(get_many_PC_loading_vectors_by_subsampling_points_dataframe(full_dataset_dataframe, n, PC_of_interest))
}

subsampled_PC_loadings_from_10k_dataset <- function(n_sample_size, PC_of_interest) {
	list_of_pc_loadings <- lapply(list_10k_dataset_hitrun_dataframes(), get_loadings_for_PC, PC_of_interest)
	return(list_of_pc_loadings)
}

##' num_replicates is the number of times that we should pull (with replacement) from the dataframe
get_many_PC_loading_vectors_by_subsampling_points_dataframe <- function(hitrun_dataframe, n_samples, PC_of_interest, num_replicates) {
	list_of_subsamples_from_df <- lapply(1:num_replicates, hitrun_dataframe[sample(nrow(hitrun_dataframe), n_samples), ])
	list_of_PC_loading_vectors <- lapply(list_of_subsamples_from_df, get_loadings_for_PC, PC_of_interest)
	return(list_of_PC_loading_vectors)
}

main()