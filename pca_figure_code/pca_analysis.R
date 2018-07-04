library(ggplot2)
library(devtools)
library(pbmcapply)
library(dplyr)
library(gridExtra)
pca_loadings_and_component_info <- function(hitrun_point_dataframe, scale=TRUE, center=TRUE) {
	res <- prcomp(hitrun_point_dataframe, scale=scale, center=center)
	return(res)
}

finger_muscle_names_in_order <- function(){
	return(c("FDP","FDS","EIP","EDC","LUM","DI","PI"))
}

list_10k_dataset_hitrun_dataframes <- function(){
	blank_col_hitrun_data <- lapply(csv_filename_list(), read.csv, header=FALSE)
	list_of_hitrun_dataframes <- lapply(blank_col_hitrun_data, add_finger_muscle_name_cols)
	return(list_of_hitrun_dataframes)
}


add_finger_muscle_name_cols<- function(hitrun_point_dataframe){
	colnames(hitrun_point_dataframe) <- finger_muscle_names_in_order()
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

pc1_pc2_varexplained_pdf_figure <- function(){
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
##' @description Each of these files has ten thousand activations that are capable of producing the force mentioned on the title. 
##' Note that these forces are in the distal direction of a human index finger.
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

0.0
3.2
6.4
9.6
12.8
16.0
19.2
22.4
25.6

list_of_strings_of_newtons <- function() c("0.0","3.20","6.40","9.60","12.8","16.0","19.2","22.4","25.6","28.8")

get_loadings_for_PC <- function(hitrun_dataframe, PC, normalize_to_max_abs_value=FALSE, scale=TRUE, center=TRUE) {
	rotations <- pca_loadings_and_component_info(hitrun_dataframe, scale, center)$rotation
	rotations_t <- t(rotations)
	subset_pcs <- rotations_t[PC,]
	res <- t(data.frame(subset_pcs))
	if (normalize_to_max_abs_value){
		normalized_res <- lapply(list(res), function(x) divide_vector_by_max_of_vectors_abs_value(x))
		return(normalized_res[[1]])
	} else {
		return(res)
	}
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

divide_vector_by_max_of_vectors_abs_value <- function(vector) vector/max(abs(vector))

#loadings_per_task is a dataframe, where each row is a different task, and the columns are the normalized PC vectors to [-1,1]
pc_loadings_parcoord <- function(loadings_per_task){
	require(reshape2) # for melt
	dfm = melt(loadings_per_task)
	pca_parcoord_plot <- ggplot(dfm, aes(variable,value,group=X1, colour=X1))+ scale_fill_brewer() + geom_path(alpha=1) + theme_bw() + geom_point()
	return(pca_parcoord_plot)
}



assimilate_vector_signs_to_reference <- function(dataframe_of_interest, reference_vector){
	require(plyr)
	
	return(adply(dataframe_of_interest, 1, flip_sign_if_dot_is_negative, reference_vector))
}

flip_sign_if_dot_is_negative <- function(vector_of_loading_values, reference_vector){
	dot_product_result <- vector_of_loading_values %*% reference_vector
	if (is_negative(dot_product_result)) {
		return(vector_of_loading_values*-1)
	} else {
		return(vector_of_loading_values)
	}
}

is_negative <- function(x) x<0
is_positive <- function(x) x>0




sorted_distal_progression_csv_filename_list <- function(){
 unsorted_files <- distal_progression_csv_filename_list()
 sorted_files <- order_filenames_by_force_number(unsorted_files)
 return(sorted_files)
}

order_filenames_by_force_number <- function(hitrun_finger_forcevector_filename_list) {
	# forces is the list of number value.
	forces <- extract_force_values(hitrun_finger_forcevector_filename_list)
	index_sorted <- sort(forces, index.return = TRUE)
	reordered_filename_list <- hitrun_finger_forcevector_filename_list[index_sorted$ix]
	return(reordered_filename_list)
}

extract_force_values <- function(hitrun_finger_forcevector_filename_list) {
	forces_strings <- lapply(hitrun_finger_forcevector_filename_list, extract_force_number_from_filename_string)
	forces <- as.numeric(do.call(c,forces_strings))
	return(forces)
}

extract_force_number_from_filename_string <- function(filename_string) {
	first_cut <- substr(filename_string, 20, nchar(filename_string))
	extracted_number <- sub("_.*$", "", first_cut)
	return(extracted_number)
}

distal_progression_csv_filename_list <- function() {
	return(
		c(
"finger_forcevector_0.0_1484767835427.csv",
"finger_forcevector_0.461446320481747_1484785251285.csv",
"finger_forcevector_0.778690665812948_1484788980242.csv",
"finger_forcevector_0.922892640963494_1484768166294.csv",
"finger_forcevector_0.951733035993603_1484768498216.csv",
"finger_forcevector_0.2307231602408735_1484782424122.csv",
"finger_forcevector_0.2595635552709827_1484782739603.csv",
"finger_forcevector_0.3749251353914194_1484784223650.csv",
"finger_forcevector_0.4326059254516378_1484784920080.csv",
"finger_forcevector_0.4902867155118562_1484785589496.csv",
"finger_forcevector_0.5191271105419654_1484785918124.csv",
"finger_forcevector_0.5479675055720745_1484786254083.csv",
"finger_forcevector_0.5768079006021837_1484786586994.csv",
"finger_forcevector_0.6056482956322929_1484786912837.csv",
"finger_forcevector_0.6344886906624021_1484787234016.csv",
"finger_forcevector_0.6633290856925113_1484787556840.csv",
"finger_forcevector_0.6921694807226205_1484787911554.csv",
"finger_forcevector_0.7210098757527297_1484788286063.csv",
"finger_forcevector_0.7498502707828388_1484788648824.csv",
"finger_forcevector_0.8075310608430573_1484789302756.csv",
"finger_forcevector_0.8363714558731664_1484789676069.csv",
"finger_forcevector_0.8652118509032756_1484790031451.csv",
"finger_forcevector_0.08652118509032757_1484770098793.csv",
"finger_forcevector_0.8940522459333847_1484767834424.csv",
"finger_forcevector_0.9805734310237124_1484770097594.csv",
"finger_forcevector_0.11536158012043675_1484779918417.csv",
"finger_forcevector_0.14420197515054592_1484780562255.csv",
"finger_forcevector_0.17304237018065513_1484781868392.csv",
"finger_forcevector_0.20188276521076431_1484782146820.csv",
"finger_forcevector_0.28840395030109184_1484783169668.csv",
"finger_forcevector_0.028840395030109187_1484768167558.csv",
"finger_forcevector_0.31724434533120105_1484783557037.csv",
"finger_forcevector_0.34608474036131026_1484783889796.csv",
"finger_forcevector_0.40376553042152863_1484784578005.csv",
"finger_forcevector_0.057680790060218375_1484768498979.csv",
"finger_forcevector_1.06709461611404_1484781868372.csv",
"finger_forcevector_1.41317935647535_1484785919542.csv",
"finger_forcevector_1.0094138260538215_1484779917822.csv",
"finger_forcevector_1.095935011144149_1484782147255.csv",
"finger_forcevector_1.240136986294695_1484783889452.csv",
"finger_forcevector_1.0382542210839307_1484780560931.csv",
"finger_forcevector_1.384338961445241_1484785590546.csv",
"finger_forcevector_1.528540936595787_1484787236313.csv",
"finger_forcevector_1.557381331625896_1484787558527.csv",
"finger_forcevector_1.845785281926988_1484768498694.csv",
"finger_forcevector_1.903466071987206_1484779917897.csv",
"finger_forcevector_1.1247754061742583_1484782424233.csv",
"finger_forcevector_1.1536158012043674_1484782739933.csv",
"finger_forcevector_1.1824561962344766_1484783170386.csv",
"finger_forcevector_1.2112965912645859_1484783557192.csv",
"finger_forcevector_1.2689773813248042_1484784222587.csv",
"finger_forcevector_1.2978177763549135_1484784578591.csv",
"finger_forcevector_1.3266581713850225_1484784920172.csv",
"finger_forcevector_1.3554985664151318_1484785252115.csv",
"finger_forcevector_1.4420197515054594_1484786255449.csv",
"finger_forcevector_1.4708601465355686_1484786590509.csv",
"finger_forcevector_1.4997005415656777_1484786914591.csv",
"finger_forcevector_1.5862217266560052_1484787914368.csv",
"finger_forcevector_1.6150621216861145_1484788289618.csv",
"finger_forcevector_1.6439025167162236_1484788654208.csv",
"finger_forcevector_1.6727429117463328_1484788985073.csv",
"finger_forcevector_1.7015833067764419_1484789308535.csv",
"finger_forcevector_1.7304237018065511_1484789683136.csv",
"finger_forcevector_1.7592640968366604_1484790037632.csv",
"finger_forcevector_1.7881044918667695_1484767835130.csv",
"finger_forcevector_1.8169448868968785_1484768166859.csv",
"finger_forcevector_1.8746256769570973_1484770097946.csv",
"finger_forcevector_1.9323064670173156_1484780562316.csv",
"finger_forcevector_1.9611468620474248_1484781868621.csv",
"finger_forcevector_1.9899872570775337_1484782147601.csv",
"finger_forcevector_2.8263587129507_1484838314906.csv",
"finger_forcevector_2.13418923222808_1484783891340.csv",
"finger_forcevector_2.018827652107643_1484782424913.csv",
"finger_forcevector_2.047668047137752_1484782741115.csv",
"finger_forcevector_2.48027397258939_1484787913558.csv",
"finger_forcevector_2.163029627258189_1484784225284.csv",
"finger_forcevector_2.191870022288298_1484784580247.csv",
"finger_forcevector_2.278391207378626_1484785591781.csv",
"finger_forcevector_2.336071997438844_1484786255717.csv",
"finger_forcevector_2.393752787499062_1484786914622.csv",
"finger_forcevector_2.451433577559281_1484787558459.csv",
"finger_forcevector_2.509114367619499_1484788288055.csv",
"finger_forcevector_2.566795157679717_1484788983014.csv",
"finger_forcevector_2.595635552709827_1484789306614.csv",
"finger_forcevector_2.624475947739936_1484789679793.csv",
"finger_forcevector_2.653316342770045_1484790035298.csv",
"finger_forcevector_2.0765084421678615_1484783171254.csv",
"finger_forcevector_2.768677922890482_1484791051127.csv",
"finger_forcevector_2.1053488371979707_1484783558469.csv",
"finger_forcevector_2.2207104173184073_1484784921924.csv",
"finger_forcevector_2.2495508123485166_1484785253530.csv",
"finger_forcevector_2.3072316024087347_1484785920311.csv",
"finger_forcevector_2.3649123924689532_1484786590330.csv",
"finger_forcevector_2.4225931825291718_1484787235931.csv",
"finger_forcevector_2.5379547626496084_1484788652472.csv",
"finger_forcevector_2.6821567378001543_1484790289032.csv",
"finger_forcevector_2.7109971328302636_1484790543166.csv",
"finger_forcevector_2.7398375278603724_1484790796792.csv",
"finger_forcevector_2.7975183179205914_1484838103945.csv",
"finger_forcevector_2.8551991079808094_1484838526515.csv",
"finger_forcevector_2.8840395030109187_1484839083626.csv",
"finger_forcevector_2.9128798980410275_1484845229140.csv",
"finger_forcevector_2.9417202930711372_1484848494180.csv",
"finger_forcevector_2.9705606881012465_1484854180606.csv",
"finger_forcevector_2.9994010831313553_1484854412512.csv",
"finger_forcevector_3.54736858870343_1484858992520.csv",
"finger_forcevector_3.057081873191574_1484854868310.csv",
"finger_forcevector_3.89345332906474_1484783170319.csv",
"finger_forcevector_3.114762663251792_1484855322305.csv",
"finger_forcevector_3.230124243372229_1484856270058.csv",
"finger_forcevector_3.258964638402338_1484856491407.csv",
"finger_forcevector_3.0282414781614646_1484854644255.csv",
"finger_forcevector_3.287805033432447_1484856728930.csv",
"finger_forcevector_3.374326218522775_1484857440528.csv",
"finger_forcevector_3.432007008582993_1484857908453.csv",
"finger_forcevector_3.518528193673321_1484858697208.csv",
"finger_forcevector_3.576208983733539_1484859256063.csv",
"finger_forcevector_3.605049378763648_1484767835473.csv",
"finger_forcevector_3.633889773793757_1484768167766.csv",
"finger_forcevector_3.691570563853976_1484770101500.csv",
"finger_forcevector_3.720410958884085_1484779920248.csv",
"finger_forcevector_3.806932143974412_1484782147506.csv",
"finger_forcevector_3.835772539004522_1484782424277.csv",
"finger_forcevector_3.0859222682216827_1484855095982.csv",
"finger_forcevector_3.864612934034631_1484782739838.csv",
"finger_forcevector_3.1436030582819017_1484855547376.csv",
"finger_forcevector_3.1724434533120105_1484855770370.csv",
"finger_forcevector_3.2012838483421198_1484855990714.csv",
"finger_forcevector_3.3166454284625564_1484856956821.csv",
"finger_forcevector_3.3454858234926657_1484857201393.csv",
"finger_forcevector_3.4031666135528837_1484857679520.csv",
"finger_forcevector_3.4608474036131023_1484858123731.csv",
"finger_forcevector_3.4896877986432115_1484858361749.csv",
"finger_forcevector_3.6627301688238667_1484768500940.csv",
"finger_forcevector_3.7492513539141945_1484780563897.csv",
"finger_forcevector_3.7780917489443033_1484781869209.csv",
"finger_forcevector_3.9222937240948497_1484783557056.csv",
"finger_forcevector_3.9511341191249585_1484783889609.csv",
"finger_forcevector_3.9799745141550673_1484784223139.csv",
"finger_forcevector_4.008814909185177_1484784578519.csv",
"finger_forcevector_4.23953806942605_1484787235969.csv",
"finger_forcevector_4.26837846445616_1484787558179.csv",
"finger_forcevector_4.037655304215286_1484784920583.csv",
"finger_forcevector_4.066495699245395_1484785252224.csv",
"finger_forcevector_4.095336094275504_1484785590005.csv",
"finger_forcevector_4.96054794517878_1484785255392.csv",
"finger_forcevector_4.124176489305614_1484785918618.csv",
"finger_forcevector_4.153016884335723_1484786254502.csv",
"finger_forcevector_4.181857279365833_1484786587643.csv",
"finger_forcevector_4.297218859486269_1484787913171.csv",
"finger_forcevector_4.326059254516378_1484788286544.csv",
"finger_forcevector_4.354899649546487_1484788650019.csv",
"finger_forcevector_4.383740044576596_1484788981184.csv",
"finger_forcevector_4.412580439606705_1484789303700.csv",
"finger_forcevector_4.441420834636815_1484789677357.csv",
"finger_forcevector_4.470261229666924_1484790033570.csv",
"finger_forcevector_4.499101624697033_1484767835182.csv",
"finger_forcevector_4.527942019727143_1484768166994.csv",
"finger_forcevector_4.556782414757252_1484768500230.csv",
"finger_forcevector_4.585622809787361_1484770101316.csv",
"finger_forcevector_4.614463204817469_1484779921015.csv",
"finger_forcevector_4.643303599847579_1484780565700.csv",
"finger_forcevector_4.672143994877688_1484781869956.csv",
"finger_forcevector_4.700984389907797_1484782148729.csv",
"finger_forcevector_4.758665179968015_1484782742245.csv",
"finger_forcevector_4.787505574998124_1484783173308.csv",
"finger_forcevector_4.816345970028235_1484783559985.csv",
"finger_forcevector_4.874026760088452_1484784226209.csv",
"finger_forcevector_4.902867155118562_1484784581008.csv",
"finger_forcevector_4.931707550148671_1484784923434.csv",
"finger_forcevector_4.989388340208889_1484785592495.csv",
"finger_forcevector_4.2106976743959414_1484786914003.csv",
"finger_forcevector_4.7298247849379065_1484782426106.csv",
"finger_forcevector_4.8451863650583435_1484783892128.csv",
"finger_forcevector_5.3354730805702_1484789680486.csv",
"finger_forcevector_5.6527174259014_1484782741147.csv",
"finger_forcevector_5.018228735238998_1484785921607.csv",
"finger_forcevector_5.30663268554009_1484789307145.csv",
"finger_forcevector_5.047069130269107_1484786256905.csv",
"finger_forcevector_5.68155782093151_1484783171589.csv",
"finger_forcevector_5.075909525299217_1484786591403.csv",
"finger_forcevector_5.104749920329326_1484786914989.csv",
"finger_forcevector_5.133590315359434_1484787236711.csv",
"finger_forcevector_5.162430710389544_1484787559239.csv",
"finger_forcevector_5.191271105419654_1484787916087.csv",
"finger_forcevector_5.220111500449763_1484788290036.csv",
"finger_forcevector_5.248951895479872_1484788654325.csv",
"finger_forcevector_5.277792290509981_1484788984187.csv",
"finger_forcevector_5.364313475600309_1484790035683.csv",
"finger_forcevector_5.393153870630417_1484767834429.csv",
"finger_forcevector_5.421994265660527_1484768167075.csv",
"finger_forcevector_5.450834660690636_1484768500155.csv",
"finger_forcevector_5.479675055720745_1484770100615.csv",
"finger_forcevector_5.537355845780964_1484780563960.csv",
"finger_forcevector_5.566196240811073_1484781869876.csv",
"finger_forcevector_5.595036635841183_1484782148162.csv",
"finger_forcevector_5.710398215961619_1484783558664.csv",
"finger_forcevector_5.739238610991728_1484783891038.csv",
"finger_forcevector_5.768079006021837_1484784224782.csv",
"finger_forcevector_5.796919401051946_1484784580129.csv",
"finger_forcevector_5.825759796082055_1484784922481.csv",
"finger_forcevector_5.854600191112165_1484785253196.csv",
"finger_forcevector_5.912280981172383_1484785919796.csv",
"finger_forcevector_5.941121376202493_1484786255366.csv",
"finger_forcevector_5.969961771232602_1484786590086.csv",
"finger_forcevector_5.998802166262711_1484786914051.csv",
"finger_forcevector_5.5085154507508545_1484779920238.csv",
"finger_forcevector_5.6238770308712915_1484782425571.csv",
"finger_forcevector_5.8834405861422745_1484785591504.csv",
"finger_forcevector_6.02764256129282_1484787235312.csv",
"finger_forcevector_6.056482956322929_1484787557795.csv",
"finger_forcevector_6.71981204201544_1484855322622.csv",
"finger_forcevector_6.74865243704555_1484855547315.csv",
"finger_forcevector_6.085323351353038_1484787913394.csv",
"finger_forcevector_6.114163746383148_1484788287475.csv",
"finger_forcevector_6.143004141413257_1484788650359.csv",
"finger_forcevector_6.171844536443365_1484788982134.csv",
"finger_forcevector_6.200684931473475_1484789304988.csv",
"finger_forcevector_6.229525326503584_1484789679029.csv",
"finger_forcevector_6.258365721533694_1484790034960.csv",
"finger_forcevector_6.287206116563803_1484790287476.csv",
"finger_forcevector_6.316046511593912_1484790541609.csv",
"finger_forcevector_6.344886906624021_1484790796132.csv",
"finger_forcevector_6.373727301654131_1484791050265.csv",
"finger_forcevector_6.431408091714348_1484838314555.csv",
"finger_forcevector_6.460248486744458_1484838525176.csv",
"finger_forcevector_6.489088881774567_1484839082567.csv",
"finger_forcevector_6.517929276804676_1484845228036.csv",
"finger_forcevector_6.575610066864894_1484854181005.csv",
"finger_forcevector_6.604450461895004_1484854412971.csv",
"finger_forcevector_6.633290856925113_1484854644777.csv",
"finger_forcevector_6.690971646985331_1484855096226.csv",
"finger_forcevector_6.777492832075659_1484855769958.csv",
"finger_forcevector_6.835173622135877_1484856270729.csv",
"finger_forcevector_6.864014017165986_1484856492416.csv",
"finger_forcevector_6.892854412196095_1484856729992.csv",
"finger_forcevector_6.921694807226205_1484856957777.csv",
"finger_forcevector_6.950535202256314_1484857203112.csv",
"finger_forcevector_6.979375597286423_1484857442023.csv",
"finger_forcevector_6.4025676966842395_1484838104107.csv",
"finger_forcevector_6.5467696718347845_1484848494944.csv",
"finger_forcevector_6.6621312519552225_1484854868667.csv",
"finger_forcevector_6.8063332271057675_1484855991033.csv",
"finger_forcevector_7.008215992316533_1484857680228.csv",
"finger_forcevector_7.09473717740686_1484858361502.csv",
"finger_forcevector_7.037056387346642_1484857909056.csv",
"finger_forcevector_7.44082191776817_1484782425719.csv",
"finger_forcevector_7.78690665812948_1484786589863.csv",
"finger_forcevector_7.123577572436969_1484858697275.csv",
"finger_forcevector_7.152417967467078_1484858992884.csv",
"finger_forcevector_7.210098757527296_1484767836000.csv",
"finger_forcevector_7.238939152557405_1484768167860.csv",
"finger_forcevector_7.267779547587514_1484768500256.csv",
"finger_forcevector_7.296619942617625_1484770101871.csv",
"finger_forcevector_7.325460337647733_1484779920879.csv",
"finger_forcevector_7.354300732677843_1484780565417.csv",
"finger_forcevector_7.383141127707952_1484781869946.csv",
"finger_forcevector_7.411981522738061_1484782148457.csv",
"finger_forcevector_7.469662312798278_1484782741778.csv",
"finger_forcevector_7.498502707828389_1484783171770.csv",
"finger_forcevector_7.527343102858498_1484783558949.csv",
"finger_forcevector_7.556183497888607_1484783891574.csv",
"finger_forcevector_7.613864287948824_1484784580203.csv",
"finger_forcevector_7.642704682978934_1484784921548.csv",
"finger_forcevector_7.0658967823767505_1484858124181.csv",
"finger_forcevector_7.671545078009044_1484785253266.csv",
"finger_forcevector_7.700385473039153_1484785590748.csv",
"finger_forcevector_7.729225868069262_1484785919388.csv",
"finger_forcevector_7.758066263099371_1484786255429.csv",
"finger_forcevector_7.815747053159589_1484786913867.csv",
"finger_forcevector_7.844587448189699_1484787235170.csv",
"finger_forcevector_7.873427843219808_1484787557661.csv",
"finger_forcevector_7.902268238249917_1484787913239.csv",
"finger_forcevector_7.931108633280026_1484788287103.csv",
"finger_forcevector_7.959949028310135_1484788651460.csv",
"finger_forcevector_7.988789423340244_1484788983056.csv",
"finger_forcevector_7.1812583624971875_1484859256414.csv",
"finger_forcevector_7.5850238929187155_1484784225194.csv",
"finger_forcevector_8.4790761388521_1484854868345.csv",
"finger_forcevector_8.10415100346068_1484790289239.csv",
"finger_forcevector_8.13299139849079_1484790543059.csv",
"finger_forcevector_8.017629818370354_1484789307003.csv",
"finger_forcevector_8.21951258358112_1484838103709.csv",
"finger_forcevector_8.046470213400463_1484789680507.csv",
"finger_forcevector_8.50791653388221_1484855096232.csv",
"finger_forcevector_8.53675692891232_1484855322057.csv",
"finger_forcevector_8.075310608430572_1484790035485.csv",
"finger_forcevector_8.82516087921341_1484857673610.csv",
"finger_forcevector_8.88284166927363_1484858114558.csv",
"finger_forcevector_8.161831793520898_1484790796765.csv",
"finger_forcevector_8.190672188551009_1484791050948.csv",
"finger_forcevector_8.248352978611228_1484838314523.csv",
"finger_forcevector_8.277193373641337_1484838526416.csv",
"finger_forcevector_8.306033768671446_1484839083709.csv",
"finger_forcevector_8.334874163701555_1484845229485.csv",
"finger_forcevector_8.363714558731665_1484848494955.csv",
"finger_forcevector_8.392554953761774_1484854181141.csv",
"finger_forcevector_8.421395348791883_1484854413913.csv",
"finger_forcevector_8.450235743821992_1484854644769.csv",
"finger_forcevector_8.565597323942429_1484855546928.csv",
"finger_forcevector_8.594437718972538_1484855768559.csv",
"finger_forcevector_8.623278114002646_1484855988638.csv",
"finger_forcevector_8.652118509032755_1484856266363.csv",
"finger_forcevector_8.680958904062864_1484856488306.csv",
"finger_forcevector_8.709799299092975_1484856724185.csv",
"finger_forcevector_8.738639694123084_1484856951261.csv",
"finger_forcevector_8.767480089153192_1484857196281.csv",
"finger_forcevector_8.796320484183301_1484857434178.csv",
"finger_forcevector_8.854001274243519_1484857901596.csv",
"finger_forcevector_8.911682064303738_1484858349403.csv",
"finger_forcevector_8.940522459333849_1484858679723.csv",
"finger_forcevector_8.969362854363958_1484858982219.csv",
"finger_forcevector_8.998203249394066_1484859243982.csv",
"finger_forcevector_9.20008601460483_1484874157557.csv",
"finger_forcevector_9.25776680466505_1484874656325.csv",
"finger_forcevector_9.027043644424175_1484859563289.csv",
"finger_forcevector_9.51733035993603_1484876819493.csv",
"finger_forcevector_9.54617075496614_1484877059973.csv",
"finger_forcevector_9.055884039454286_1484859878446.csv",
"finger_forcevector_9.63269194005647_1484877855695.csv",
"finger_forcevector_9.084724434484395_1484860204500.csv",
"finger_forcevector_9.89225549532745_1484880005327.csv",
"finger_forcevector_9.92109589035756_1484880208012.csv",
"finger_forcevector_9.94993628538767_1484880444997.csv",
"finger_forcevector_9.113564829514504_1484862021916.csv",
"finger_forcevector_9.142405224544612_1484873668542.csv",
"finger_forcevector_9.171245619574721_1484873909204.csv",
"finger_forcevector_9.228926409634939_1484874406545.csv",
"finger_forcevector_9.286607199695158_1484874900581.csv",
"finger_forcevector_9.315447594725267_1484875139468.csv",
"finger_forcevector_9.344287989755376_1484875379619.csv",
"finger_forcevector_9.373128384785485_1484875620160.csv",
"finger_forcevector_9.401968779815594_1484875860410.csv",
"finger_forcevector_9.430809174845704_1484876101259.csv",
"finger_forcevector_9.459649569875813_1484876341281.csv",
"finger_forcevector_9.488489964905922_1484876580306.csv",
"finger_forcevector_9.575011149996248_1484877321119.csv",
"finger_forcevector_9.603851545026359_1484877594192.csv",
"finger_forcevector_9.661532335086578_1484878095692.csv",
"finger_forcevector_9.690372730116687_1484878335018.csv",
"finger_forcevector_9.719213125146796_1484878573631.csv",
"finger_forcevector_9.748053520176905_1484878812741.csv",
"finger_forcevector_9.776893915207015_1484879051660.csv",
"finger_forcevector_9.805734310237124_1484879289886.csv",
"finger_forcevector_9.834574705267233_1484879528862.csv",
"finger_forcevector_9.863415100297342_1484879767093.csv",
"finger_forcevector_9.978776680417779_1484880684196.csv",
"finger_forcevector_10.6709461611404_1484886467621.csv",
"finger_forcevector_10.007617075447888_1484880923990.csv",
"finger_forcevector_10.9881905064716_1484874167645.csv",
"finger_forcevector_10.23834023568876_1484882836019.csv",
"finger_forcevector_10.29602102574898_1484883313575.csv",
"finger_forcevector_10.036457470477997_1484881162767.csv",
"finger_forcevector_10.61326537108018_1484885991712.csv",
"finger_forcevector_10.64210576611029_1484886228535.csv",
"finger_forcevector_10.065297865508105_1484881401145.csv",
"finger_forcevector_10.93050971641138_1484873678676.csv",
"finger_forcevector_10.094138260538214_1484881639967.csv",
"finger_forcevector_10.95935011144149_1484873918263.csv",
"finger_forcevector_10.122978655568325_1484881879361.csv",
"finger_forcevector_10.151819050598434_1484882119182.csv",
"finger_forcevector_10.180659445628542_1484882358965.csv",
"finger_forcevector_10.209499840658651_1484882597929.csv",
"finger_forcevector_10.267180630718869_1484883075120.csv",
"finger_forcevector_10.324861420779088_1484883550924.csv",
"finger_forcevector_10.353701815809199_1484883791895.csv",
"finger_forcevector_10.382542210839308_1484884029996.csv",
"finger_forcevector_10.411382605869417_1484884268088.csv",
"finger_forcevector_10.440223000899525_1484884531009.csv",
"finger_forcevector_10.469063395929636_1484884780521.csv",
"finger_forcevector_10.497903790959745_1484885020100.csv",
"finger_forcevector_10.526744185989854_1484885273852.csv",
"finger_forcevector_10.555584581019962_1484885514701.csv",
"finger_forcevector_10.584424976050071_1484885752615.csv",
"finger_forcevector_10.699786556170508_1484886705453.csv",
"finger_forcevector_10.728626951200617_1484886943988.csv",
"finger_forcevector_10.757467346230726_1484887203622.csv",
"finger_forcevector_10.786307741260835_1484887477395.csv",
"finger_forcevector_10.815148136290945_1484859575966.csv",
"finger_forcevector_10.843988531321054_1484859890702.csv",
"finger_forcevector_10.872828926351163_1484860215824.csv",
"finger_forcevector_10.901669321381272_1484862170041.csv",
"finger_forcevector_11.3054348518028_1484876830543.csv",
"finger_forcevector_11.017030901501709_1484874415856.csv",
"finger_forcevector_11.33427524683291_1484877071217.csv",
"finger_forcevector_11.36311564186302_1484877334898.csv",
"finger_forcevector_11.045871296531818_1484874665870.csv",
"finger_forcevector_11.65151959216411_1484879777752.csv",
"finger_forcevector_11.70920038222433_1484880216643.csv",
"finger_forcevector_11.074711691561928_1484874909941.csv",
"finger_forcevector_11.103552086592037_1484875149652.csv",
"finger_forcevector_11.132392481622146_1484875390615.csv",
"finger_forcevector_11.161232876652255_1484875631858.csv",
"finger_forcevector_11.190073271682365_1484875872571.csv",
"finger_forcevector_11.218913666712474_1484876112538.csv",
"finger_forcevector_11.247754061742583_1484876353128.csv",
"finger_forcevector_11.276594456772692_1484876591641.csv",
"finger_forcevector_11.391956036893129_1484877608583.csv",
"finger_forcevector_11.420796431923238_1484877866907.csv",
"finger_forcevector_11.449636826953347_1484878105444.csv",
"finger_forcevector_11.478477221983455_1484878344635.csv",
"finger_forcevector_11.507317617013564_1484878583772.csv",
"finger_forcevector_11.536158012043675_1484878822874.csv",
"finger_forcevector_11.564998407073784_1484879061309.csv",
"finger_forcevector_11.593838802103893_1484879299047.csv",
"finger_forcevector_11.622679197134001_1484879539123.csv",
"finger_forcevector_11.680359987194219_1484880015576.csv",
"finger_forcevector_11.738040777254438_1484880455368.csv",
"finger_forcevector_11.766881172284549_1484880694109.csv",
"finger_forcevector_11.795721567314658_1484880932720.csv",
"finger_forcevector_11.824561962344767_1484881171848.csv",
"finger_forcevector_11.853402357374875_1484881410460.csv",
"finger_forcevector_11.882242752404986_1484881650106.csv",
"finger_forcevector_11.911083147435095_1484881889418.csv",
"finger_forcevector_11.939923542465204_1484882128558.csv",
"finger_forcevector_11.968763937495313_1484882367015.csv",
"finger_forcevector_11.997604332525421_1484882605695.csv",
"finger_forcevector_12.02644472755553_1484882843626.csv",
"finger_forcevector_12.05528512258564_1484883082605.csv",
"finger_forcevector_12.08412551761575_1484883321043.csv",
"finger_forcevector_12.34368907288673_1484885520808.csv",
"finger_forcevector_12.37252946791684_1484885760229.csv",
"finger_forcevector_12.40136986294695_1484885998890.csv",
"finger_forcevector_12.71861420827815_1484873676339.csv",
"finger_forcevector_12.77629499833837_1484874164227.csv",
"finger_forcevector_12.112965912645858_1484883559123.csv",
"finger_forcevector_12.141806307675967_1484883798652.csv",
"finger_forcevector_12.170646702706076_1484884037364.csv",
"finger_forcevector_12.199487097736185_1484884275014.csv",
"finger_forcevector_12.228327492766295_1484884538129.csv",
"finger_forcevector_12.257167887796404_1484884787881.csv",
"finger_forcevector_12.286008282826513_1484885026967.csv",
"finger_forcevector_12.314848677856622_1484885281243.csv",
"finger_forcevector_12.430210257977059_1484886237355.csv",
"finger_forcevector_12.459050653007168_1484886475845.csv",
"finger_forcevector_12.487891048037278_1484886714010.csv",
"finger_forcevector_12.516731443067387_1484886952121.csv",
"finger_forcevector_12.545571838097496_1484887212487.csv",
"finger_forcevector_12.574412233127607_1484887484994.csv",
"finger_forcevector_12.603252628157716_1484859575840.csv",
"finger_forcevector_12.632093023187824_1484859889623.csv",
"finger_forcevector_12.660933418217933_1484860214056.csv",
"finger_forcevector_12.689773813248042_1484862041604.csv",
"finger_forcevector_12.747454603308261_1484873915321.csv",
"finger_forcevector_12.805135393368479_1484874412132.csv",
"finger_forcevector_12.833975788398588_1484874662330.csv",
"finger_forcevector_12.862816183428697_1484874907148.csv",
"finger_forcevector_12.891656578458806_1484875147209.csv",
"finger_forcevector_12.920496973488916_1484875387636.csv",
"finger_forcevector_12.949337368519025_1484875628502.csv",
"finger_forcevector_12.978177763549134_1484875868967.csv",
"finger_forcevector_13.4973048740911_1484859575061.csv",
"finger_forcevector_13.06469894863946_1484876588693.csv",
"finger_forcevector_13.007018158579243_1484876108819.csv",
"finger_forcevector_13.8145492194223_1484875387760.csv",
"finger_forcevector_13.12237973869968_1484877068293.csv",
"finger_forcevector_13.035858553609351_1484876349555.csv",
"finger_forcevector_13.43962408403088_1484879773468.csv",
"finger_forcevector_13.46846447906099_1484880012094.csv",
"finger_forcevector_13.75686842936208_1484874907176.csv",
"finger_forcevector_13.78570882439219_1484875147217.csv",
"finger_forcevector_13.84338961445241_1484875629272.csv",
"finger_forcevector_13.093539343669569_1484876827695.csv",
"finger_forcevector_13.151220133729788_1484877330255.csv",
"finger_forcevector_13.180060528759897_1484877603708.csv",
"finger_forcevector_13.208900923790008_1484877863249.csv",
"finger_forcevector_13.237741318820117_1484878102141.csv",
"finger_forcevector_13.266581713850226_1484878341364.csv",
"finger_forcevector_13.295422108880336_1484878580499.csv",
"finger_forcevector_13.324262503910445_1484878818646.csv",
"finger_forcevector_13.353102898940554_1484879057162.csv",
"finger_forcevector_13.381943293970663_1484879295281.csv",
"finger_forcevector_13.410783689000771_1484879535257.csv",
"finger_forcevector_13.526145269121209_1484859889728.csv",
"finger_forcevector_13.554985664151317_1484860214894.csv",
"finger_forcevector_13.583826059181426_1484862168339.csv",
"finger_forcevector_13.612666454211535_1484873677288.csv",
"finger_forcevector_13.641506849241646_1484873916879.csv",
"finger_forcevector_13.670347244271754_1484874164996.csv",
"finger_forcevector_13.699187639301863_1484874413271.csv",
"finger_forcevector_13.728028034331972_1484874662888.csv",
"finger_forcevector_13.872230009482518_1484875870429.csv",
"finger_forcevector_13.901070404512629_1484876110986.csv",
"finger_forcevector_13.929910799542737_1484876351333.csv",
"finger_forcevector_13.958751194572846_1484876590077.csv",
"finger_forcevector_13.987591589602957_1484876829677.csv",
"finger_forcevector_14.016431984633066_1484877069351.csv",
"finger_forcevector_14.18947435481372_1484878582214.csv",
"finger_forcevector_14.21831474984383_1484878820923.csv",
"finger_forcevector_14.045272379663174_1484877332647.csv",
"finger_forcevector_14.47787830511481_1484768499002.csv",
"finger_forcevector_14.59323988523525_1484781869964.csv",
"finger_forcevector_14.074112774693283_1484877606497.csv",
"finger_forcevector_14.88164383553634_1484785255116.csv",
"finger_forcevector_14.91048423056645_1484785591896.csv",
"finger_forcevector_14.102953169723392_1484877864481.csv",
"finger_forcevector_14.131793564753501_1484878103762.csv",
"finger_forcevector_14.160633959783612_1484878342920.csv",
"finger_forcevector_14.247155144873938_1484879060410.csv",
"finger_forcevector_14.275995539904047_1484879298326.csv",
"finger_forcevector_14.304835934934156_1484879537107.csv",
"finger_forcevector_14.333676329964266_1484879775916.csv",
"finger_forcevector_14.362516724994375_1484880014299.csv",
"finger_forcevector_14.391357120024484_1484880216608.csv",
"finger_forcevector_14.420197515054593_1484767835181.csv",
"finger_forcevector_14.449037910084703_1484768166801.csv",
"finger_forcevector_14.506718700144921_1484770099309.csv",
"finger_forcevector_14.535559095175028_1484779919905.csv",
"finger_forcevector_14.564399490205139_1484780564179.csv",
"finger_forcevector_14.622080280265358_1484782148615.csv",
"finger_forcevector_14.650920675295467_1484782426192.csv",
"finger_forcevector_14.679761070325576_1484782742564.csv",
"finger_forcevector_14.708601465355686_1484783173399.csv",
"finger_forcevector_14.737441860385793_1484783560425.csv",
"finger_forcevector_14.766282255415904_1484783892243.csv",
"finger_forcevector_14.795122650446014_1484784226479.csv",
"finger_forcevector_14.823963045476122_1484784581713.csv",
"finger_forcevector_14.852803440506232_1484784923708.csv",
"finger_forcevector_14.939324625596557_1484785920294.csv",
"finger_forcevector_14.968165020626667_1484786256029.csv",
"finger_forcevector_14.997005415656778_1484786590730.csv",
"finger_forcevector_15.25656897092776_1484789679531.csv",
"finger_forcevector_15.025845810686885_1484786914436.csv",
"finger_forcevector_15.054686205716996_1484787236146.csv",
"finger_forcevector_15.57381331625896_1484848495239.csv",
"finger_forcevector_15.60265371128907_1484854181638.csv",
"finger_forcevector_15.083526600747103_1484787558191.csv",
"finger_forcevector_15.91989805662027_1484856729630.csv",
"finger_forcevector_15.94873845165038_1484856956871.csv",
"finger_forcevector_15.112366995777213_1484787913985.csv",
"finger_forcevector_15.141207390807324_1484788288016.csv",
"finger_forcevector_15.170047785837431_1484788651439.csv",
"finger_forcevector_15.198888180867542_1484788982874.csv",
"finger_forcevector_15.227728575897649_1484789306098.csv",
"finger_forcevector_15.285409365957868_1484790035133.csv",
"finger_forcevector_15.314249760987979_1484790289011.csv",
"finger_forcevector_15.343090156018087_1484790543550.csv",
"finger_forcevector_15.371930551048196_1484790796610.csv",
"finger_forcevector_15.400770946078307_1484791050981.csv",
"finger_forcevector_15.429611341108414_1484838104115.csv",
"finger_forcevector_15.458451736138525_1484838314155.csv",
"finger_forcevector_15.487292131168635_1484838526199.csv",
"finger_forcevector_15.516132526198742_1484839084877.csv",
"finger_forcevector_15.544972921228853_1484845230897.csv",
"finger_forcevector_15.631494106319177_1484854413560.csv",
"finger_forcevector_15.660334501349288_1484854645417.csv",
"finger_forcevector_15.689174896379399_1484854868952.csv",
"finger_forcevector_15.718015291409506_1484855096113.csv",
"finger_forcevector_15.746855686439616_1484855322256.csv",
"finger_forcevector_15.775696081469723_1484855546801.csv",
"finger_forcevector_15.804536476499834_1484855770219.csv",
"finger_forcevector_15.833376871529945_1484855991508.csv",
"finger_forcevector_15.862217266560052_1484856270697.csv",
"finger_forcevector_15.891057661590162_1484856492281.csv",
"finger_forcevector_15.977578846680489_1484857201042.csv",
"finger_forcevector_16.0064192417106_1484857439025.csv",
"finger_forcevector_16.640907932373_1484876110692.csv",
"finger_forcevector_16.9581522777042_1484878820722.csv",
"finger_forcevector_16.20830200692136_1484859255763.csv",
"finger_forcevector_16.26598279698158_1484859890201.csv",
"finger_forcevector_16.29482319201169_1484860215602.csv",
"finger_forcevector_16.35250398207191_1484873678543.csv",
"finger_forcevector_16.035259636740708_1484857678475.csv",
"finger_forcevector_16.43902516716224_1484874414474.csv",
"finger_forcevector_16.61206753734289_1484875870699.csv",
"finger_forcevector_16.064100031770817_1484857908044.csv",
"finger_forcevector_16.66974832740311_1484876350621.csv",
"finger_forcevector_16.72742911746333_1484876828229.csv",
"finger_forcevector_16.87163109261387_1484878103487.csv",
"finger_forcevector_16.092940426800926_1484858122639.csv",
"finger_forcevector_16.98699267273431_1484879060181.csv",
"finger_forcevector_16.121780821831035_1484858360907.csv",
"finger_forcevector_16.150621216861143_1484858695557.csv",
"finger_forcevector_16.179461611891256_1484858991344.csv",
"finger_forcevector_16.237142401951473_1484859575442.csv",
"finger_forcevector_16.323663587041796_1484862169500.csv",
"finger_forcevector_16.381344377102018_1484873917526.csv",
"finger_forcevector_16.410184772132126_1484874166477.csv",
"finger_forcevector_16.467865562192344_1484874664171.csv",
"finger_forcevector_16.496705957222456_1484874908053.csv",
"finger_forcevector_16.525546352252565_1484875148134.csv",
"finger_forcevector_16.554386747282674_1484875388436.csv",
"finger_forcevector_16.583227142312783_1484875629744.csv",
"finger_forcevector_16.698588722433218_1484876589478.csv",
"finger_forcevector_16.756269512493436_1484877068656.csv",
"finger_forcevector_16.785109907523548_1484877331772.csv",
"finger_forcevector_16.813950302553653_1484877605436.csv",
"finger_forcevector_16.842790697583766_1484877864403.csv",
"finger_forcevector_16.900471487643983_1484878343069.csv",
"finger_forcevector_16.929311882674092_1484878581875.csv",
"finger_forcevector_17.01583306776442_1484879298677.csv",
"finger_forcevector_17.07351385782464_1484879777624.csv",
"finger_forcevector_17.10235425285475_1484880015230.csv",
"finger_forcevector_17.30423701806551_1484881652031.csv",
"finger_forcevector_17.39075820315584_1484882369535.csv",
"finger_forcevector_17.41959859818595_1484882608344.csv",
"finger_forcevector_17.044673462794528_1484879538586.csv",
"finger_forcevector_17.65032175842682_1484884542883.csv",
"finger_forcevector_17.73684294351715_1484885285099.csv",
"finger_forcevector_17.76568333854726_1484885525726.csv",
"finger_forcevector_17.131194647884858_1484880218238.csv",
"finger_forcevector_17.160035042914966_1484880456972.csv",
"finger_forcevector_17.188875437945075_1484880695801.csv",
"finger_forcevector_17.217715832975184_1484880935998.csv",
"finger_forcevector_17.246556228005293_1484881175195.csv",
"finger_forcevector_17.275396623035405_1484881413230.csv",
"finger_forcevector_17.333077413095623_1484881891040.csv",
"finger_forcevector_17.361917808125728_1484882130166.csv",
"finger_forcevector_17.448438993216058_1484882847551.csv",
"finger_forcevector_17.477279388246167_1484883086763.csv",
"finger_forcevector_17.506119783276276_1484883324690.csv",
"finger_forcevector_17.534960178306385_1484883561901.csv",
"finger_forcevector_17.563800573336493_1484883802888.csv",
"finger_forcevector_17.592640968366602_1484884041675.csv",
"finger_forcevector_17.621481363396715_1484884279420.csv",
"finger_forcevector_17.679162153456932_1484884792093.csv",
"finger_forcevector_17.708002548487038_1484885030946.csv",
"finger_forcevector_17.794523733577368_1484885765012.csv",
"finger_forcevector_17.823364128607476_1484886003552.csv",
"finger_forcevector_17.852204523637585_1484886241498.csv",
"finger_forcevector_17.881044918667698_1484886480512.csv",
"finger_forcevector_17.909885313697803_1484886719322.csv",
"finger_forcevector_17.938725708727915_1484886957838.csv",
"finger_forcevector_17.967566103758024_1484887220735.csv",
"finger_forcevector_17.996406498788133_1484887492656.csv",
"finger_forcevector_18.02524689381824_1484790291031.csv",
"finger_forcevector_18.5155336093301_1484855770458.csv",
"finger_forcevector_18.05408728884835_1484790545946.csv",
"finger_forcevector_18.08292768387846_1484790799778.csv",
"finger_forcevector_18.8327779546613_1484858361012.csv",
"finger_forcevector_18.11176807890857_1484791052558.csv",
"finger_forcevector_18.16944886896879_1484838314876.csv",
"finger_forcevector_18.37133163417955_1484854644823.csv",
"finger_forcevector_18.40017202920966_1484854868949.csv",
"finger_forcevector_18.42901242423977_1484855097309.csv",
"finger_forcevector_18.74625676957097_1484857678965.csv",
"finger_forcevector_18.86161834969141_1484858694812.csv",
"finger_forcevector_18.140608473938677_1484838105372.csv",
"finger_forcevector_18.198289263998895_1484838525903.csv",
"finger_forcevector_18.227129659029007_1484839084024.csv",
"finger_forcevector_18.255970054059112_1484845230512.csv",
"finger_forcevector_18.284810449089225_1484848495918.csv",
"finger_forcevector_18.313650844119334_1484854181332.csv",
"finger_forcevector_18.342491239149442_1484854413596.csv",
"finger_forcevector_18.457852819269878_1484855323433.csv",
"finger_forcevector_18.486693214299986_1484855547887.csv",
"finger_forcevector_18.544374004360208_1484855991593.csv",
"finger_forcevector_18.573214399390316_1484856270969.csv",
"finger_forcevector_18.602054794420425_1484856492753.csv",
"finger_forcevector_18.630895189450534_1484856730068.csv",
"finger_forcevector_18.659735584480647_1484856957426.csv",
"finger_forcevector_18.688575979510752_1484857201773.csv",
"finger_forcevector_18.717416374540864_1484857440089.csv",
"finger_forcevector_18.775097164601082_1484857908037.csv",
"finger_forcevector_18.803937559631187_1484858123611.csv",
"finger_forcevector_18.890458744721517_1484858989937.csv",
"finger_forcevector_18.919299139751626_1484859254178.csv",
"finger_forcevector_18.948139534781735_1484859575062.csv",
"finger_forcevector_18.976979929811844_1484859890243.csv",
"finger_forcevector_19.03466071987206_1484862167743.csv",
"finger_forcevector_19.4672666453237_1484877071072.csv",
"finger_forcevector_19.005820324841956_1484860214914.csv",
"finger_forcevector_19.7845109906549_1484879776510.csv",
"finger_forcevector_19.09234150993228_1484873917679.csv",
"finger_forcevector_19.12118190496239_1484874166042.csv",
"finger_forcevector_19.17886269502261_1484874664714.csv",
"finger_forcevector_19.26538388011294_1484875389119.csv",
"finger_forcevector_19.49610704035381_1484877335109.csv",
"finger_forcevector_19.52494743538392_1484877608411.csv",
"finger_forcevector_19.55378783041403_1484877865469.csv",
"finger_forcevector_19.61146862047425_1484878344428.csv",
"finger_forcevector_19.063501114902174_1484873677775.csv",
"finger_forcevector_19.81335138568501_1484880015227.csv",
"finger_forcevector_19.84219178071512_1484880218313.csv",
"finger_forcevector_19.89987257077534_1484880696300.csv",
"finger_forcevector_19.92871296580545_1484880935707.csv",
"finger_forcevector_19.150022299992496_1484874414876.csv",
"finger_forcevector_19.207703090052718_1484874908981.csv",
"finger_forcevector_19.236543485082827_1484875149122.csv",
"finger_forcevector_19.294224275143044_1484875630194.csv",
"finger_forcevector_19.323064670173157_1484875870912.csv",
"finger_forcevector_19.351905065203265_1484876112194.csv",
"finger_forcevector_19.380745460233374_1484876352358.csv",
"finger_forcevector_19.409585855263483_1484876591436.csv",
"finger_forcevector_19.438426250293592_1484876830086.csv",
"finger_forcevector_19.582628225444136_1484878104714.csv",
"finger_forcevector_19.640309015504354_1484878582871.csv",
"finger_forcevector_19.669149410534466_1484878822291.csv",
"finger_forcevector_19.697989805564575_1484879060528.csv",
"finger_forcevector_19.726830200594684_1484879298287.csv",
"finger_forcevector_19.755670595624792_1484879538131.csv",
"finger_forcevector_19.871032175745228_1484880456755.csv",
"finger_forcevector_19.957553360835558_1484881174202.csv",
"finger_forcevector_19.986393755865667_1484881412883.csv",
"finger_forcevector_20.13059573101621_1484882608153.csv",
"finger_forcevector_20.015234150895775_1484881651753.csv",
"finger_forcevector_20.18827652107643_1484883085671.csv",
"finger_forcevector_20.21711691610654_1484883324733.csv",
"finger_forcevector_20.24595731113665_1484883562792.csv",
"finger_forcevector_20.27479770616676_1484883803855.csv",
"finger_forcevector_20.044074545925888_1484881891310.csv",
"finger_forcevector_20.47668047137752_1484885525547.csv",
"finger_forcevector_20.56320165646785_1484886240964.csv",
"finger_forcevector_20.59204205149796_1484886480152.csv",
"finger_forcevector_20.072914940955993_1484882130285.csv",
"finger_forcevector_20.88044600179905_1484888866927.csv",
"finger_forcevector_20.90928639682916_1484889098004.csv",
"finger_forcevector_20.99580758191949_1484889791085.csv",
"finger_forcevector_20.101755335986105_1484882369152.csv",
"finger_forcevector_20.159436126046323_1484882846444.csv",
"finger_forcevector_20.303638101196867_1484884042622.csv",
"finger_forcevector_20.332478496226976_1484884281144.csv",
"finger_forcevector_20.361318891257085_1484884544874.csv",
"finger_forcevector_20.390159286287194_1484884793678.csv",
"finger_forcevector_20.418999681317302_1484885032293.csv",
"finger_forcevector_20.447840076347415_1484885286333.csv",
"finger_forcevector_20.505520866407632_1484885763702.csv",
"finger_forcevector_20.534361261437738_1484886002969.csv",
"finger_forcevector_20.620882446528068_1484886718537.csv",
"finger_forcevector_20.649722841558177_1484886957157.csv",
"finger_forcevector_20.678563236588285_1484887218946.csv",
"finger_forcevector_20.707403631618398_1484887490082.csv",
"finger_forcevector_20.736244026648503_1484887718706.csv",
"finger_forcevector_20.765084421678615_1484887948331.csv",
"finger_forcevector_20.793924816708724_1484888175876.csv",
"finger_forcevector_20.822765211738833_1484888405006.csv",
"finger_forcevector_20.851605606768942_1484888635660.csv",
"finger_forcevector_20.938126791859272_1484889330354.csv",
"finger_forcevector_20.966967186889377_1484889560586.csv",
"finger_forcevector_21.659136667612_1484790542990.csv",
"finger_forcevector_21.3418923222808_1484891680422.csv",
"finger_forcevector_21.9763810129432_1484854648123.csv",
"finger_forcevector_21.19769034713025_1484891277925.csv",
"finger_forcevector_21.22653074216036_1484891357881.csv",
"finger_forcevector_21.024647976949595_1484890022139.csv",
"finger_forcevector_21.25537113719047_1484891437465.csv",
"finger_forcevector_21.28421153222058_1484891516639.csv",
"finger_forcevector_21.053488371979707_1484890252109.csv",
"finger_forcevector_21.57261548252167_1484892331417.csv",
"finger_forcevector_21.63029627258189_1484790289112.csv",
"finger_forcevector_21.68797706264211_1484790797035.csv",
"finger_forcevector_21.082328767009813_1484890482676.csv",
"finger_forcevector_21.86101943282276_1484845232372.csv",
"finger_forcevector_21.91870022288298_1484854184831.csv",
"finger_forcevector_21.94754061791309_1484854417066.csv",
"finger_forcevector_21.111169162039925_1484890712936.csv",
"finger_forcevector_21.140009557070034_1484890940175.csv",
"finger_forcevector_21.168849952100143_1484891128001.csv",
"finger_forcevector_21.313051927250687_1484891597559.csv",
"finger_forcevector_21.370732717310908_1484891760963.csv",
"finger_forcevector_21.399573112341017_1484891841797.csv",
"finger_forcevector_21.428413507371125_1484891921878.csv",
"finger_forcevector_21.457253902401234_1484892004883.csv",
"finger_forcevector_21.486094297431347_1484892085796.csv",
"finger_forcevector_21.514934692461452_1484892166522.csv",
"finger_forcevector_21.543775087491564_1484892247793.csv",
"finger_forcevector_21.601455877551782_1484892412949.csv",
"finger_forcevector_21.716817457672217_1484791052175.csv",
"finger_forcevector_21.745657852702326_1484838105073.csv",
"finger_forcevector_21.774498247732435_1484838315215.csv",
"finger_forcevector_21.803338642762544_1484838527919.csv",
"finger_forcevector_21.832179037792656_1484839086273.csv",
"finger_forcevector_21.889859827852874_1484848498537.csv",
"finger_forcevector_22.00522140797331_1484854871738.csv",
"finger_forcevector_22.2936253582744_1484857203250.csv",
"finger_forcevector_22.6108697036056_1484860213390.csv",
"finger_forcevector_22.32246575330451_1484857441687.csv",
"finger_forcevector_22.034061803003418_1484855099863.csv",
"finger_forcevector_22.35130614833462_1484857680213.csv",
"finger_forcevector_22.38014654336473_1484857908288.csv",
"finger_forcevector_22.43782733342495_1484858360388.csv",
"finger_forcevector_22.062902198033527_1484855326178.csv",
"finger_forcevector_22.63971009863571_1484862039059.csv",
"finger_forcevector_22.66855049366582_1484873675237.csv",
"finger_forcevector_22.72623128372604_1484874164274.csv",
"finger_forcevector_22.091742593063636_1484855551049.csv",
"finger_forcevector_22.95695444396691_1484876110175.csv",
"finger_forcevector_22.120582988093744_1484855772920.csv",
"finger_forcevector_22.149423383123857_1484855993361.csv",
"finger_forcevector_22.178263778153966_1484856272841.csv",
"finger_forcevector_22.207104173184074_1484856494282.csv",
"finger_forcevector_22.235944568214183_1484856731707.csv",
"finger_forcevector_22.264784963244292_1484856959199.csv",
"finger_forcevector_22.408986938394836_1484858122879.csv",
"finger_forcevector_22.466667728455054_1484858693660.csv",
"finger_forcevector_22.495508123485166_1484858989959.csv",
"finger_forcevector_22.524348518515275_1484859254725.csv",
"finger_forcevector_22.553188913545384_1484859574923.csv",
"finger_forcevector_22.582029308575493_1484859889297.csv",
"finger_forcevector_22.697390888695928_1484873914837.csv",
"finger_forcevector_22.755071678756146_1484874412617.csv",
"finger_forcevector_22.783912073786258_1484874662633.csv",
"finger_forcevector_22.812752468816367_1484874907671.csv",
"finger_forcevector_22.841592863846476_1484875148009.csv",
"finger_forcevector_22.870433258876588_1484875388445.csv",
"finger_forcevector_22.899273653906693_1484875629769.csv",
"finger_forcevector_22.928114048936806_1484875870108.csv",
"finger_forcevector_22.985794838997023_1484876349587.csv",
"finger_forcevector_23.01463523402713_1484876588486.csv",
"finger_forcevector_23.04347562905724_1484876827322.csv",
"finger_forcevector_23.07231602408735_1484877068031.csv",
"finger_forcevector_23.10115641911746_1484877330344.csv",
"finger_forcevector_23.30303918432822_1484879059467.csv",
"finger_forcevector_23.38956036941855_1484879775787.csv",
"finger_forcevector_23.41840076444866_1484880013912.csv",
"finger_forcevector_23.70680471474975_1484882366769.csv",
"finger_forcevector_23.73564510977986_1484882604982.csv",
"finger_forcevector_23.82216629487019_1484883321822.csv",
"finger_forcevector_23.129996814147567_1484877603624.csv",
"finger_forcevector_23.158837209177676_1484877863021.csv",
"finger_forcevector_23.187677604207785_1484878102845.csv",
"finger_forcevector_23.216517999237897_1484878342998.csv",
"finger_forcevector_23.245358394268003_1484878581776.csv",
"finger_forcevector_23.274198789298115_1484878820285.csv",
"finger_forcevector_23.331879579358333_1484879297326.csv",
"finger_forcevector_23.360719974388438_1484879536400.csv",
"finger_forcevector_23.447241159478768_1484880216792.csv",
"finger_forcevector_23.476081554508877_1484880455141.csv",
"finger_forcevector_23.504921949538986_1484880694212.csv",
"finger_forcevector_23.533762344569098_1484880933745.csv",
"finger_forcevector_23.562602739599207_1484881172485.csv",
"finger_forcevector_23.591443134629316_1484881411257.csv",
"finger_forcevector_23.620283529659424_1484881650038.csv",
"finger_forcevector_23.649123924689533_1484881889859.csv",
"finger_forcevector_23.677964319719642_1484882128581.csv",
"finger_forcevector_23.764485504809972_1484882844580.csv",
"finger_forcevector_23.793325899840077_1484883083618.csv",
"finger_forcevector_23.851006689900295_1484883559678.csv",
"finger_forcevector_23.879847084930407_1484883800271.csv",
"finger_forcevector_23.908687479960516_1484884038867.csv",
"finger_forcevector_23.937527874990625_1484884278530.csv",
"finger_forcevector_23.966368270020734_1484884541616.csv",
"finger_forcevector_23.995208665050843_1484884791368.csv",
"finger_forcevector_24.1682510352315_1484886237840.csv",
"finger_forcevector_24.02404906008095_1484885029145.csv",
"finger_forcevector_24.4854953805627_1484888866823.csv",
"finger_forcevector_24.05288945511106_1484885283471.csv",
"finger_forcevector_24.8027397258939_1484891278765.csv",
"finger_forcevector_24.08172985014117_1484885522449.csv",
"finger_forcevector_24.11057024517128_1484885761127.csv",
"finger_forcevector_24.39897419547237_1484888174273.csv",
"finger_forcevector_24.45665498553259_1484888635514.csv",
"finger_forcevector_24.51433577559281_1484889097584.csv",
"finger_forcevector_24.68737814577346_1484890484115.csv",
"finger_forcevector_24.74505893583368_1484890940755.csv",
"finger_forcevector_24.77389933086379_1484891129558.csv",
"finger_forcevector_24.83158012092401_1484891358629.csv",
"finger_forcevector_24.139410640201387_1484885999997.csv",
"finger_forcevector_24.197091430261608_1484886476611.csv",
"finger_forcevector_24.225931825291717_1484886715368.csv",
"finger_forcevector_24.254772220321826_1484886953282.csv",
"finger_forcevector_24.283612615351934_1484887215288.csv",
"finger_forcevector_24.312453010382047_1484887487030.csv",
"finger_forcevector_24.341293405412152_1484887716040.csv",
"finger_forcevector_24.370133800442265_1484887945156.csv",
"finger_forcevector_24.427814590502482_1484888404408.csv",
"finger_forcevector_24.543176170622917_1484889328986.csv",
"finger_forcevector_24.572016565653026_1484889561066.csv",
"finger_forcevector_24.600856960683135_1484889791536.csv",
"finger_forcevector_24.629697355713244_1484890021596.csv",
"finger_forcevector_24.658537750743356_1484890253157.csv",
"finger_forcevector_24.716218540803574_1484890713797.csv",
"finger_forcevector_24.860420515954118_1484891438221.csv",
"finger_forcevector_24.889260910984227_1484891517876.csv",
"finger_forcevector_24.918101306014336_1484891598931.csv",
"finger_forcevector_24.946941701044445_1484891682126.csv",
"finger_forcevector_24.975782096074557_1484891762942.csv",
"finger_forcevector_25.1199840712251_1484892169438.csv",
"finger_forcevector_25.4372284165563_1484882130143.csv",
"finger_forcevector_25.004622491104666_1484891844019.csv",
"finger_forcevector_25.17766486128532_1484892334475.csv",
"finger_forcevector_25.20650525631543_1484892415365.csv",
"finger_forcevector_25.26418604637565_1484880694530.csv",
"finger_forcevector_25.033462886134775_1484891924224.csv",
"finger_forcevector_25.46606881158641_1484882369199.csv",
"finger_forcevector_25.55258999667674_1484883085054.csv",
"finger_forcevector_25.062303281164883_1484892007509.csv",
"finger_forcevector_25.78331315691761_1484885031289.csv",
"finger_forcevector_25.86983434200794_1484885763848.csv",
"finger_forcevector_25.89867473703805_1484886002713.csv",
"finger_forcevector_25.091143676194992_1484892088648.csv",
"finger_forcevector_25.92751513206816_1484886241168.csv",
"finger_forcevector_25.148824466255213_1484892250839.csv",
"finger_forcevector_25.235345651345536_1484880455140.csv",
"finger_forcevector_25.293026441405754_1484880934126.csv",
"finger_forcevector_25.321866836435866_1484881172889.csv",
"finger_forcevector_25.350707231465975_1484881411909.csv",
"finger_forcevector_25.379547626496084_1484881649920.csv",
"finger_forcevector_25.408388021526193_1484881890369.csv",
"finger_forcevector_25.494909206616523_1484882607589.csv",
"finger_forcevector_25.523749601646628_1484882846518.csv",
"finger_forcevector_25.581430391706846_1484883323656.csv",
"finger_forcevector_25.610270786736958_1484883562053.csv",
"finger_forcevector_25.639111181767067_1484883802447.csv",
"finger_forcevector_25.667951576797176_1484884041155.csv",
"finger_forcevector_25.696791971827285_1484884280229.csv",
"finger_forcevector_25.725632366857393_1484884543391.csv",
"finger_forcevector_25.754472761887506_1484884792414.csv",
"finger_forcevector_25.812153551947723_1484885284787.csv",
"finger_forcevector_25.840993946977832_1484885525026.csv",
"finger_forcevector_25.956355527098268_1484886480128.csv",
"finger_forcevector_25.985195922128376_1484886718394.csv",
"finger_forcevector_26.9946097481822_1484891278545.csv",
"finger_forcevector_26.12939789727892_1484887714639.csv",
"finger_forcevector_26.014036317158485_1484886956860.csv",
"finger_forcevector_26.21591908236925_1484888400808.csv",
"finger_forcevector_26.24475947739936_1484888630386.csv",
"finger_forcevector_26.042876712188598_1484887218965.csv",
"finger_forcevector_26.53316342770045_1484890928073.csv",
"finger_forcevector_26.56200382273056_1484887949185.csv",
"finger_forcevector_26.64852500782089_1484888637901.csv",
"finger_forcevector_26.071717107218703_1484887490876.csv",
"finger_forcevector_26.85040777303165_1484890252760.csv",
"finger_forcevector_26.87924816806176_1484890483898.csv",
"finger_forcevector_26.90808856309187_1484890714900.csv",
"finger_forcevector_26.93692895812198_1484890941505.csv",
"finger_forcevector_26.100557502248815_1484887721580.csv",
"finger_forcevector_26.158238292309033_1484887943340.csv",
"finger_forcevector_26.187078687339138_1484888172667.csv",
"finger_forcevector_26.273599872429468_1484888861156.csv",
"finger_forcevector_26.302440267459577_1484889091423.csv",
"finger_forcevector_26.331280662489686_1484889321051.csv",
"finger_forcevector_26.360121057519795_1484889551218.csv",
"finger_forcevector_26.388961452549907_1484889780664.csv",
"finger_forcevector_26.417801847580016_1484890010413.csv",
"finger_forcevector_26.446642242610125_1484890240641.csv",
"finger_forcevector_26.475482637640233_1484890470291.csv",
"finger_forcevector_26.504323032670342_1484890699948.csv",
"finger_forcevector_26.590844217760672_1484888177904.csv",
"finger_forcevector_26.619684612790778_1484888407780.csv",
"finger_forcevector_26.677365402850995_1484888868485.csv",
"finger_forcevector_26.706205797881108_1484889100055.csv",
"finger_forcevector_26.735046192911216_1484889331233.csv",
"finger_forcevector_26.763886587941325_1484889561230.csv",
"finger_forcevector_26.792726982971434_1484889791856.csv",
"finger_forcevector_26.821567378001543_1484890022490.csv",
"finger_forcevector_26.965769353152087_1484891129361.csv",
"finger_forcevector_27.3118540935134_1484882845071.csv",
"finger_forcevector_27.6290984388446_1484885522623.csv",
"finger_forcevector_27.9463427841758_1484887943156.csv",
"finger_forcevector_27.22533290842307_1484882128107.csv",
"finger_forcevector_27.023450143212308_1484880455182.csv",
"finger_forcevector_27.28301369848329_1484882605960.csv",
"finger_forcevector_27.34069448854351_1484883083624.csv",
"finger_forcevector_27.42721567363384_1484883800292.csv",
"finger_forcevector_27.51373685872416_1484884541363.csv",
"finger_forcevector_27.052290538242417_1484880693562.csv",
"finger_forcevector_27.57141764878438_1484885029090.csv",
"finger_forcevector_27.60025804381449_1484885283120.csv",
"finger_forcevector_27.65793883387471_1484885761253.csv",
"finger_forcevector_27.68677922890482_1484885999778.csv",
"finger_forcevector_27.081130933272526_1484880933663.csv",
"finger_forcevector_27.109971328302635_1484881172485.csv",
"finger_forcevector_27.138811723332747_1484881410667.csv",
"finger_forcevector_27.167652118362852_1484881649018.csv",
"finger_forcevector_27.196492513392965_1484881888675.csv",
"finger_forcevector_27.254173303453182_1484882366905.csv",
"finger_forcevector_27.369534883573618_1484883321220.csv",
"finger_forcevector_27.398375278603726_1484883559878.csv",
"finger_forcevector_27.456056068663944_1484884039868.csv",
"finger_forcevector_27.484896463694056_1484884277717.csv",
"finger_forcevector_27.542577253754274_1484884790874.csv",
"finger_forcevector_27.715619623934927_1484886237640.csv",
"finger_forcevector_27.744460018965036_1484886477344.csv",
"finger_forcevector_27.773300413995145_1484886715261.csv",
"finger_forcevector_27.802140809025257_1484886954191.csv",
"finger_forcevector_27.830981204055366_1484887216130.csv",
"finger_forcevector_27.859821599085475_1484887487759.csv",
"finger_forcevector_27.888661994115584_1484887718097.csv",
"finger_forcevector_27.917502389145692_1484887715366.csv",
"finger_forcevector_27.975183179205914_1484888170884.csv",
"finger_forcevector_28.00402357423602_1484888401469.csv",
"finger_forcevector_28.03286396926613_1484888632414.csv",
"finger_forcevector_28.09054475932635_1484889095229.csv",
"finger_forcevector_28.29242752453711_1484890711363.csv",
"finger_forcevector_28.35010831459733_1484891127342.csv",
"finger_forcevector_28.37894870962744_1484887946918.csv",
"finger_forcevector_28.43662949968766_1484888403852.csv",
"finger_forcevector_28.60967186986831_1484889787068.csv",
"finger_forcevector_28.061704364296236_1484888863328.csv",
"finger_forcevector_28.69619305495864_1484890479385.csv",
"finger_forcevector_28.72503344998875_1484890709568.csv",
"finger_forcevector_28.75387384501886_1484890936682.csv",
"finger_forcevector_28.119385154356454_1484889328011.csv",
"finger_forcevector_28.148225549386567_1484889560297.csv",
"finger_forcevector_28.177065944416675_1484889789729.csv",
"finger_forcevector_28.205906339446784_1484890020485.csv",
"finger_forcevector_28.234746734476893_1484890251677.csv",
"finger_forcevector_28.263587129507002_1484890481848.csv",
"finger_forcevector_28.321267919567223_1484890938807.csv",
"finger_forcevector_28.407789104657546_1484888175024.csv",
"finger_forcevector_28.465469894717767_1484888634269.csv",
"finger_forcevector_28.494310289747876_1484888864988.csv",
"finger_forcevector_28.523150684777985_1484889095174.csv",
"finger_forcevector_28.551991079808094_1484889326828.csv",
"finger_forcevector_28.580831474838206_1484889557398.csv",
"finger_forcevector_28.638512264898424_1484890018074.csv",
"finger_forcevector_28.667352659928532_1484890249802.csv",
"finger_forcevector_28.782714240048968_1484891124713.csv",
"finger_forcevector_28.811554635079077_1484891275183.csv"
)
)
}

#for many files in one directory
get_list_of_point_matrices <- function(filename_list, folder_path){
  list_of_point_matrices <- lapply(
   filename_list,
   function(i){
     read.csv(paste0(folder_path,i), header=FALSE)
   })
  return(list_of_point_matrices)
}

add_col_with_value <- function(dataframe, value, colname="force") {
  dataframe_new <- data.frame(dataframe)
  dataframe_new[colname] <- rep(value, length(dataframe[,1]))
  colnames(dataframe_new) <- c(colnames(dataframe), colname)
  return(dataframe_new)
}



#in this case I am using FDP, so that's column 1.
multiply_rows_by_sign_of_column <- function(pc_df, col_to_use_as_reference = 1){
  #apply transformation
  pc_mult_by_col_sign <-  apply(pc_df,1,function(x) {
    fdp_loading_value <- as.numeric(x[col_to_use_as_reference])
    return(as.numeric(x) * sign(fdp_loading_value))
  })
  #add back the column names
  pc_df_fdp <- data.frame(t(pc_mult_by_col_sign))
  colnames(pc_df_fdp) <- c(finger_muscle_names_in_order(), "force")
  #make sure force is always positive.
  pc_df_fdp$force <- abs(pc_df_fdp$force)
  return(pc_df_fdp)
}


plot_loadings_by_muscle <- function(pc1_df, pc2_df, pc3_df, ...) {
lapply(1:7, function(x) {
plot(pc1_df$force, pc3_df[,x],type='n', ylim=c(-1,1),main="", ylab="")
lines(pc1_df$force, pc1_df[,x], ylim=c(-1,1),main="", col='blue',...)
lines(pc2_df$force, pc2_df[,x], ylim=c(-1,1),main="", col='black',...)
lines(pc3_df$force, pc3_df[,x], ylim=c(-1,1),main="", col='purple',...)
})
}



get_3loadings_dataframe <- function(point_matrices_labeled, center, scale){


pc1_loadings <- pbmclapply(point_matrices_labeled, get_loadings_for_PC,1, center=center, scale=scale, mc.cores=8)
pc1_loadings_and_forces<- lapply(1:length(pc1_loadings), function(index) {
  add_col_with_value(pc1_loadings[[index]], force_levels[index])
})
pc1_df <- do.call('rbind', pc1_loadings_and_forces)


pc2_loadings <- pbmclapply(point_matrices_labeled, get_loadings_for_PC,2, center=center, scale=scale, mc.cores=8)
pc2_loadings_and_forces<- lapply(1:length(pc2_loadings), function(index) {
  add_col_with_value(pc2_loadings[[index]], force_levels[index])
})
pc2_df <- do.call('rbind', pc2_loadings_and_forces)

pc3_loadings <- pbmclapply(point_matrices_labeled, get_loadings_for_PC,3, center=center, scale=scale, mc.cores=8)
pc3_loadings_and_forces<- lapply(1:length(pc3_loadings), function(index) {
  add_col_with_value(pc3_loadings[[index]], force_levels[index])
})
pc3_df <- do.call('rbind', pc3_loadings_and_forces)
pc1_df_FDP <- multiply_rows_by_sign_of_column(pc1_df, 1)
pc1_df_FDP$pc <- factor(1, levels=c(1:3))
pc2_df_FDP <- multiply_rows_by_sign_of_column(pc2_df, 1)
pc2_df_FDP$pc <- factor(2, levels=c(1:3))
pc3_df_FDP <- multiply_rows_by_sign_of_column(pc3_df, 1)
pc3_df_FDP$pc <- factor(3, levels=c(1:3))
df_pcs <- rbind(pc1_df_FDP,pc2_df_FDP,pc3_df_FDP)
attr(df_pcs,"raw") <- list(pc1_df=pc1_df,
pc2_df=pc2_df,
pc3_df=pc3_df)
return(df_pcs)
}

gen_loadings_pca_permutations <- function(point_matrices_labeled){
	df_pcsCS <- get_3loadings_dataframe(point_matrices_labeled, center=TRUE, scale=TRUE)
	df_pcsCS$pca_params <- "center and scale"
	df_pcsS <- get_3loadings_dataframe(point_matrices_labeled, center=FALSE, scale=TRUE)
	df_pcsS$pca_params <- "scale"
	df_pcsC <- get_3loadings_dataframe(point_matrices_labeled, center=TRUE, scale=FALSE)
	df_pcsC$pca_params <- "center"
	df_pcs <- get_3loadings_dataframe(point_matrices_labeled, center=FALSE, scale=FALSE)
	df_pcs$pca_params <- "raw"
	df_pcs$pca_params <- "raw"
	df_pcs <- rbind(df_pcsCS,df_pcsS,df_pcsC,df_pcs)
	return(df_pcs)
}


