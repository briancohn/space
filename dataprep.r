task_force_80 <- read.csv('finger_forcevector_23.01463523402713_1484876588486.csv', header=FALSE)
named_muscles_and_costs <- c("FDP","FDS","EIP","EDC","LUM","DI","PI","L1","L2","L1W","L2W")
fmax_vector <- c(123.0, 219.0,	23.52, 91.74,	21.6,	124.8,129.6)
df_with_costs <- fill_costs(task_force_80,m=7, fmax_vector)
colnames(df_with_costs) <-  named_muscles_and_costs
fill_costs <- function(df,m=7, fmax_vector) {
  F0 = matrix(fmax_vector,dim(df)[1],m,byrow=TRUE)
  # L1
  df[,(m+1)] = rowSums(df[,1:m])
  # L2
  df[,(m+2)] = (rowSums(df[,1:m]^2))^(1/2)
  # Lw1
  df[,(m+3)] = rowSums(df[,1:m]*F0)
  # Lw2
  df[,(m+4)] = (rowSums((df[,1:m]*F0)^2))^(1/2)
  return(df)
}

browser()
