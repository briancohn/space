	library(gridExtra)
library(MASS)

source('pca_analysis.R')

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
	scaled_pc1_loadings_list <- adply(pc1_loadings_list, 1, function(x) divide_vector_by_max_of_vectors_abs_value(x))
	scaled_pc2_loadings_list <- adply(pc2_loadings_list, 1, function(x) divide_vector_by_max_of_vectors_abs_value(x))
	pc1_plot <- pc_loadings_parcoord(scaled_pc1_loadings_list) +
	 labs(y="Normalized Loading for PC1", x="") +
	 scale_colour_discrete(name  ="Distal Fingertip Force", labels=c("3.2N", "19.2N", "25.6N")) + ylim(-1.0,1.0)
	pc2_plot <- pc_loadings_parcoord(scaled_pc2_loadings_list) +
	labs(x = "Muscle", y="Normalized Loading for PC2")+
	 scale_colour_discrete(name  ="Distal Fingertip Force", labels=c("3.2N", "19.2N", "25.6N")) +
	 ylim(-1.0,1)

	combined_plot <- grid.arrange(pc1_plot,pc2_plot, ncol=1)
	ggsave('pca_loadings_as_force_increases.pdf', combined_plot, width=12, height = 7, units="in")


}
main()