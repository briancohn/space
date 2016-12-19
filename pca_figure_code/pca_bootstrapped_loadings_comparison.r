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

list_10k_dataset_hitrun_dataframes <- function(){
	blank_col_hitrun_data <- lapply(csv_filename_list(), read.csv, header=FALSE)
	list_of_hitrun_dataframes <- lapply(blank_col_hitrun_data, add_finger_muscle_name_cols)
	return(list_of_hitrun_dataframes)
}

main <- function() {
	require(ggplot2)
	p <- ggplot(mtcars, aes(factor(cyl), mpg))
	p + geom_boxplot(aes(fill = factor(am)))
	#where each dataframe has many rows, each of which represents a PC loading vector.
	# list_of_PC_loadings_dataframes <- produce_bootstrap_pca_experiment(n=10, num_replicates=100, PC_of_interest=1)
	num_replicates=100
	num_samples = 10
	list_of_10k_hitrun_dataframes <- list_10k_dataset_hitrun_dataframes()
	list_of_PC_loading_bootstrap_lists <- lapply(list_of_10k_hitrun_dataframes,
												produce_smaller_subsample_dataframes,
												num_replicates,
												num_samples
												)
	browser()

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
	browser()
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