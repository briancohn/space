source('pca_analysis.R')
##' Do Call Rbind on list of dataframes
##' @param list of dataframes
##' @return df row-bound concatenated dataframe
dcrb <- function(list_of_dataframes) {
  do.call("rbind", list_of_dataframes)
}

library(reshape2)
library(magrittr)
source('pca_analysis.R')
task_intensities <- c(0,3.2,6.4,9.6,12.8, 16.0,19.2,22.4,25.6)
list_of_hitrun_dataframes_for_different_forces <- list_10k_dataset_hitrun_dataframes()[c(1,2,3,4,5,6,7,8,9)]
pcas_1 <- lapply(list_of_hitrun_dataframes_for_different_forces, get_loadings_for_PC, PC=1, normalize_to_max_abs_value=TRUE) %>% dcrb %>% as.data.frame
pcas_2 <- lapply(list_of_hitrun_dataframes_for_different_forces, get_loadings_for_PC, PC=2, normalize_to_max_abs_value=TRUE) %>% dcrb %>% as.data.frame
pcas_3 <- lapply(list_of_hitrun_dataframes_for_different_forces, get_loadings_for_PC, PC=3, normalize_to_max_abs_value=TRUE) %>% dcrb %>% as.data.frame

posflip_pcas1 <- apply(pcas_1[,1:7], 1, flip_sign_if_dot_is_negative, reference_vector=t(pcas_1[1,1:7]%>%as.matrix)) %>% t %>% as.data.frame
posflip_pcas2 <- apply(pcas_2[,1:7], 1, flip_sign_if_dot_is_negative, reference_vector=t(pcas_2[1,1:7]%>%as.matrix)) %>% t %>% as.data.frame
posflip_pcas3 <- apply(pcas_3[,1:7], 1, flip_sign_if_dot_is_negative, reference_vector=t(pcas_3[1,1:7]%>%as.matrix)) %>% t %>% as.data.frame

posflip_pcas1$task_intensity <- task_intensities
posflip_pcas1$PC <- "PC1"
posflip_pcas2$task_intensity <- task_intensities
posflip_pcas2$PC <- "PC2"
posflip_pcas3$task_intensity <- task_intensities
posflip_pcas3$PC <- "PC3"

pc_df <- melt(dcrb(list(posflip_pcas1,posflip_pcas2,posflip_pcas3)), id.vars=c("PC", "task_intensity"))
colnames(pc_df) <- c("PC", "task_intensity", "muscle", "normalized_loading_value")

p <- ggplot(pc_df, aes(task_intensity, normalized_loading_value, col=muscle)) + geom_line(size=1.3) + facet_grid(~PC) + theme_classic()
p <- p + theme_classic() + xlab("Task intensity in distal direction (N)") + ylab("Normalized Loading Value")





sd_per_task<- lapply(list_of_hitrun_dataframes_for_different_forces, function(x){
	res <- apply(x,2,sd)
}) %>% dcrb %>% as.data.frame
sd_per_task$task_intensity <- task_intensities
sd_df <- melt(sd_per_task, id.vars=c("task_intensity"))
colnames(sd_df) <- c("task_intensity", "muscle","sd_across_10k_solutions")

p_sd <- ggplot(sd_df, aes(task_intensity, sd_across_10k_solutions, col=muscle)) + geom_line(size=1.2)
p_sd <- p_sd + theme_classic() + xlab("Task intensity in distal direction (N)") + ylab("SD of muscle activation across\n10,000 samples of a given task intensity")
p_sd <- p_sd + geom_point()
ggsave("muscle_variance_over_tasks.png",p_sd,width=8, height=3)

zscore_the_columns <- function(df){
	apply(df, 2, function(x){
		x_mean <- mean(x)
		x_sd <- sd(x)
		z_scores <- (x - x_mean)/ x_sd
		return(z_scores)
	}) %>% as.data.frame
}

sample_har_set <- list_of_hitrun_dataframes_for_different_forces[[1]]
list_of_har_dfs_z_scores <- lapply(list_of_hitrun_dataframes_for_different_forces, zscore_the_columns)

expect_zero_mean_variance_1 <- lapply(list_of_har_dfs_z_scores, function(x){
	sds <- apply(x, 2, sd)
	means <- apply(x, 2, mean)
	list(sds,means)
})
expect_true(sum(sapply(expect_zero_mean_variance_1, function(x) x[[2]])) < 1e-12)

#================
unit_variance_df_pc1 <- lapply(list_of_har_dfs_z_scores, get_loadings_for_PC, 1, normalize_to_max_abs_value=FALSE) %>% dcrb %>% as.data.frame
unit_variance_df_pc2 <- lapply(list_of_har_dfs_z_scores, get_loadings_for_PC, 2, normalize_to_max_abs_value=FALSE) %>% dcrb %>% as.data.frame
unit_variance_df_pc3 <- lapply(list_of_har_dfs_z_scores, get_loadings_for_PC, 3, normalize_to_max_abs_value=FALSE) %>% dcrb %>% as.data.frame

posflip_pc1 <- apply(unit_variance_df_pc1[,1:7], 1, flip_sign_if_dot_is_negative, reference_vector=t(unit_variance_df_pc1[1,1:7]%>%as.matrix)) %>% t %>% as.data.frame
posflip_pc2 <- apply(unit_variance_df_pc2[,1:7], 1, flip_sign_if_dot_is_negative, reference_vector=t(unit_variance_df_pc2[1,1:7]%>%as.matrix)) %>% t %>% as.data.frame
posflip_pc3 <- apply(unit_variance_df_pc3[,1:7], 1, flip_sign_if_dot_is_negative, reference_vector=t(unit_variance_df_pc3[1,1:7]%>%as.matrix)) %>% t %>% as.data.frame

posflip_pc1$PC <- "PC1"
posflip_pc2$PC <- "PC2"
posflip_pc3$PC <- "PC3"
posflip_pc1$task_intensity <- task_intensities
posflip_pc2$task_intensity <- task_intensities
posflip_pc3$task_intensity <- task_intensities

pc_df_zeroscore <- melt(dcrb(list(posflip_pc1,posflip_pc2,posflip_pc3)), id.vars=c("PC", "task_intensity"))
colnames(pc_df_zeroscore) <- c("PC","task_intensity", "muscle","z_score_based_pc_val")
p_z_score <- ggplot(pc_df_zeroscore, aes(task_intensity, z_score_based_pc_val, col=muscle)) + geom_line(size=1.3) + facet_grid(~PC)
p_z_score <- p_z_score + theme_classic() + xlab("Task intensity in distal direction (N)") + ylab("PC loading using Z-scores as input)")
#================
G <- arrangeGrob(grobs=list(p, p_z_score), ncol=1)
ggsave('loadings_z_score.png', G, height=10,width=7) 


#TODO work on
lapply(list_of_hitrun_dataframes_for_different_forces, cov)