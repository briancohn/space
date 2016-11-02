# plot point lines
points = function(df,alpha) {
  # number of columns
  m  = dim(df)[2]
  
  # define transparency and margins
  color_transparent <- adjustcolor(col='blue', alpha.f = alpha)
  
  #plot points
  plot(colnames(df)[1:m], df[1:2,1:m], type='l', col=color_transparent, ylim=c(0.0,1.1), 
              lwd=0.2, axes=FALSE,ann=FALSE)
  
  # n = number of rows
  N = dim(df)[1]
  for (i in 2:N) {
    lines(colnames(df)[1:m], df[i,1:m], type='l', lwd=0.2, col=color_transparent, ylim=c(0.0,1.1))
  }
}

pdf_plot = function(df,alpha,finger=FALSE) {
  # start pdf
  pdf(file="Downloads/figure.pdf", height=2.3, width=4,compress=FALSE)
  # plot lines
  par(mar=c(0,0,0,0), mgp=c(0,0,0))
  points(df,alpha)
  # plot axes
  axes(df)
  # label axes
  label_axes(df,finger)
  # flush out pdf
  dev.off()
}
