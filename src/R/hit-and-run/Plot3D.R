# Note: need library(rgl)
make_3d = function(file,N) {
  # read and take sample of N rows of data
  data = read.csv(file)
  rows = sample(dim(data)[1],size=N)
  data = data[rows,]
  
  # plot in 3d space
  open3d(scale=c(1,1,1))
  segments3d(x=as.vector(data[,1]),
             y=as.vector(data[,2]),
             z=as.vector(data[,3]),
             lwd=0.001)
  plot3d(data[,1], data[,2], data[,3], type = "s", col = "red", add=TRUE, radius=0.01)
  axes3d()
  title3d(xlab="M1", ylab="M2", zlab="M3")
}

