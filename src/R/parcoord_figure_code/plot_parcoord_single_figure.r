# fills raw cost columns (14-19 for finger)
# section 3 of dataframe
fill_costs = function(df,m, fmax_vector) {
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

fmax_vector=c(123, 219, 124.8, 129.6, 23.52, 21.6, 91.74) #newtons each muscle is maximally capable of producing.
points_dataframe <- read.csv('n_1000_alphalen_10/finger_forcevector_19.207703090052718_1484767085612.csv', header=FALSE)
points_with_costs <- fill_costs(points_dataframe, 7, fmax_vector)
write.csv(points_with_costs, 'points_with_costs_for_19.2077N_distal_task.csv')

