	library(gridExtra)
library(MASS)

source('pca_analysis.R')

get_loadings_for_PC <- function(hitrun_dataframe, PC) {
	rotations <- pca_loadings_and_component_info(hitrun_dataframe)$rotation
	rotations_t <- t(rotations)
	subset_pcs <- rotations_t[PC,]
	return(t(data.frame(subset_pcs)))
}


#source http://stats.stackexchange.com/questions/178626/how-to-normalize-data-between-1-and-1
x_scaled_to_0_1 <- function(x,vector_max, vector_min) {
	return((x-vector_min) / (vector_max - vector_min))
}

#source http://stats.stackexchange.com/questions/178626/how-to-normalize-data-between-1-and-1
scale_value_between_plus_or_minus_1 <- function(x, vector_max, vector_min){
	return(2 * x_scaled_to_0_1(x, vector_max, vector_min) - 1)
}
scale_vector_between_plus_or_minus_1 <- function(vector) {
	vector_max <- max(vector)
	vector_min <- min(vector)
	scaled_list <- lapply(vector, function(x) scale_value_between_plus_or_minus_1(x,vector_max,vector_min))
	return(do.call(c, scaled_list))
}

#loadings_per_task is a dataframe, where each row is a different task, and the columns are the normalized PC vectors to [-1,1]
pc_loadings_parcoord <- function(loadings_per_task){
	require(reshape2) # for melt
	dfm = melt(loadings_per_task)
	pca_parcoord_plot <- ggplot(dfm, aes(variable,value,group=X1, colour=X1)) + geom_path(alpha=1) + theme_bw() + geom_point()
	return(pca_parcoord_plot)
}


main <- function(){

	blank_col_hitrun_data <- lapply(csv_filename_list(), read.csv, header=FALSE)
	list_of_hitrun_dataframes <- lapply(blank_col_hitrun_data, add_finger_muscle_name_cols)
	list_of_pc1_loadings <- lapply(list_of_hitrun_dataframes, get_loadings_for_PC, 1)
	list_of_pc2_loadings <- lapply(list_of_hitrun_dataframes, get_loadings_for_PC, 2)

	pc1_loadings_at_3_newtons <- list_of_pc1_loadings[[2]]
	row.names(pc1_loadings_at_3_newtons) <- "pc1_loadings_at_3_newtons"
	pc1_loadings_at_19_newtons <- list_of_pc1_loadings[[8]]
	row.names(pc1_loadings_at_19_newtons) <- "pc1_loadings_at_19_newtons"
	pc1_loadings_at_25_newtons <- list_of_pc1_loadings[[10]]
	row.names(pc1_loadings_at_25_newtons) <- "pc1_loadings_at_25_newtons"
	pc2_loadings_at_3_newtons <- list_of_pc2_loadings[[2]]
	row.names(pc2_loadings_at_3_newtons) <- "pc2_loadings_at_3_newtons"
	pc2_loadings_at_19_newtons <- list_of_pc2_loadings[[8]]
	row.names(pc2_loadings_at_19_newtons) <- "pc2_loadings_at_19_newtons"
	pc2_loadings_at_25_newtons <- list_of_pc2_loadings[[10]]
	row.names(pc2_loadings_at_25_newtons) <- "pc2_loadings_at_25_newtons"

	pc1_loadings_list <- do.call(rbind, list(
		pc1_loadings_at_3_newtons,
		pc1_loadings_at_19_newtons,
		pc1_loadings_at_25_newtons))
	pc2_loadings_list <- do.call(rbind, list(
		pc2_loadings_at_3_newtons,
		pc2_loadings_at_19_newtons,
		pc2_loadings_at_25_newtons))

	require(plyr)
	scaled_pc1_loadings_list <- adply(pc1_loadings_list, 1, function(x) scale_vector_between_plus_or_minus_1(x))
	scaled_pc2_loadings_list <- adply(pc2_loadings_list, 1, function(x) scale_vector_between_plus_or_minus_1(x))
	pc1_plot <- pc_loadings_parcoord(scaled_pc1_loadings_list) +
	 labs(y="Normalized Loading for PC1", x="") +
	 scale_colour_discrete(name  ="Distal Fingertip Force",
                            labels=c("3.2N", "19.2N", "25.6N"))
	pc2_plot <- pc_loadings_parcoord(scaled_pc2_loadings_list) +
	labs(x = "Muscle", y="Normalized Loading for PC2")+
	 scale_colour_discrete(name  ="Distal Fingertip Force",
                            labels=c("3.2N", "19.2N", "25.6N"))
	
	combined_plot <- grid.arrange(pc1_plot,pc2_plot, ncol=1)
	ggsave('pca_loadings_as_force_increases.pdf', combined_plot, width=6, height = 3.5, units="in")


}
main()