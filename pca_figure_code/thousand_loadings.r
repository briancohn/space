library(plotly)
library(magrittr)
source('pca_analysis.R')
library(memoise)
library(pbmcapply)
library(reshape2)

files <- sorted_distal_progression_csv_filename_list()
list_of_point_matrices <- get_list_of_point_matrices(files, "n_1000_alphalen_1000/")
force_levels <- do.call("c", lapply(files, extract_force_number_from_filename_string))
point_matrices_labeled <- lapply(list_of_point_matrices, add_finger_muscle_name_cols)


# melt
df_pcs <- gen_loadings_pca_permutations(point_matrices_labeled)
tall_df_pcs <- melt(df_pcs, id.vars = c("force", "pc", "pca_params"))
colnames(tall_df_pcs) <- c("force", "pc", "pca_params", "muscle", "loading_value")


loading <- ggplot(tall_df_pcs, aes(force, loading_value)) + geom_line(aes(col = pc), size = 0.9) +
    facet_grid(muscle ~ pca_params) + theme_classic()
ggplotly(loading)
ggsave("loadings_over_different_preprocessing_params.jpg",loading)

p_abs <- ggplot(tall_df_pcs, aes(force, abs(loading_value))) + geom_line(aes(col = pc), size = 0.9) +
    facet_grid(muscle ~ pca_params) + theme_classic()
ggsave("loadings_over_different_preprocessing_params_absolute_value_result.jpg",p_abs)

#look at derivative of scaled centered PCA
standard_loading_trajectories <- df_pcs[df_pcs$pca_params == "center and scale",]
diff_matrix <- apply(standard_loading_trajectories[,1:7],2,diff) %>% as.data.frame
diff_matrix$force <- standard_loading_trajectories$force[-1]
diff_matrix$pc <- standard_loading_trajectories$pc[-1]
tall_diff_matrix <- melt(diff_matrix, id.vars=c("force","pc"))
colnames(tall_diff_matrix) <- c("force","pc","muscle","loading_value")
p <- ggplot(tall_diff_matrix, aes(force, loading_value, col=pc)) + geom_line(aes(col=pc), size=0.4) + facet_grid(~muscle)
p <- p + theme_classic()
p
ggsave("differential_for_centered_and_scaled.jpg", p)
# original plot for paper
# par(mfrow=c(7,1)
# plot_loadings_by_muscle(pc1_df_FDP, pc2_df_FDP, pc3_df_FDP, lwd = 2)