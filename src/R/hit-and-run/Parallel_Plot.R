# Parallel Plot
# makes parallel coordinate plots using csv output muscle data
# to create plot, call the function make_plot

require(tools)

# returns an extended dataframe from original data
# make 3 sections: 1= data, 2= normalized costs, 3= raw costs
make_dataframe = function(data) {
  #m = dim(data)[2]
  #colnames(data) = 1:(m+12)
  return(data)
}


# fills raw cost columns (14-19 for finger)
# section 3 of dataframe
fill_costs = function(df,fmax) {
  m  = dim(df)[2] - 12
  
  # L1 (col 14)
  df[,(m+7)] = rowSums(df[,1:m])
  
  # L2 (col 15)
  df[,(m+8)] = (rowSums(df[,1:m]^2))^(1/2)
  
  # L3 (col 16)
  df[,(m+9)] = (rowSums(df[,1:m]^3))^(1/3)
  
  
  F0 = matrix(fmax,dim(df)[1],m,byrow=TRUE)
  
  # Lw1 (col 17)
  df[,(m+10)] = rowSums(df[,1:m]*F0)
  
  # Lw2 (col 18)
  df[,(m+11)] = (rowSums((df[,1:m]*F0)^2))^(1/2)
  
  # Lw3 (col 19)
  df[,(m+12)] = (rowSums((df[,1:m]*F0)^3 ))^(1/3)
  
  return(df)
}


# fills adjusted axes columns (8-13 for finger)
# normalizes cost columns
# section 2 of dataframe
fill_axes = function(df) {
  m  = dim(df)[2] - 12
  
  for (i in (m+1):(m+6)) {
    # subtract by lowest
    low = min(df[,(i+6)])
    df[,i] = df[,(i+6)] - low
    
    # divided by new highest
    #browser()
    high = max(df[,i])
    
    if (high > 0) {
      df[,i] = df[,i] / high
    }
  }
  
  return(df)
}


# plot point lines
add_paracoord_points = function(df,alpha) {
  m  = dim(df)[2]
  
  # define transparency and margins
  color_transparent <- adjustcolor(col='blue', alpha.f = alpha)
  
  #plot points
  colnames(df) = 1:m
  plot(colnames(df), df[1,1:m], type='l', col=color_transparent, ylim=c(0.0,1.1), 
       lwd=0.2, axes=FALSE,ann=FALSE)
  
  N = dim(df)[1]
  for(i in 2:N) {
    lines(colnames(df), df[i,], type='l', lwd=0.2, col=color_transparent, ylim=c(0.0,1.1))
  }
}


# calculate the axes bounds and labels
get_axis_bounds = function(df,axnum) {
  # calculate labels
  lower = min(df[,(axnum+6)])
  upper = max(df[,(axnum+6)])
  
  # find appropriate well-spaced labels
  scale = if (upper < 10) 10 else 1
  
  n=5
  llabel = trunc(lower*scale)/scale + (1/scale)
  inc = trunc((upper-llabel)*scale/n)/scale 
  ulabel = llabel + (n*inc)
  
  if (upper-ulabel > inc) {
    n = 4
    inc = trunc((upper-llabel)*scale/n)/scale 
    ulabel = llabel + (n*inc)
  }
  dig = if (upper < 10) 1 else 0
  labels = formatC(seq(llabel,ulabel,inc), format="f", digits=dig, width=5)
  
  # calculate normalized coordinates
  lcoord = (llabel-lower) / (upper-lower)
  ucoord = (ulabel-lower) / (upper-lower)
  inccoord = (ucoord-lcoord) / n
  
  return(list("low"=lcoord, "high"=ucoord, "inc"=inccoord, "labels"=labels))
}


# plot axes
create_axes = function(df, finger=FALSE){
  m  = dim(df)[2]
  
  # create custom axis for each muscle
  for (axnum in 1:m) {
    axis(2, at=seq(0,1,0.2), pos=axnum, las=2, lwd=0.2, tck=-0.005, cex.axis=0.2, hadj=1.5)
  }
  
  label_axes(df, finger=finger)
}

# label the axes
label_axes = function(df,finger=FALSE) {
  m  = dim(df)[2]
  
  axis(3, at=1:m, lwd=0, cex.axis=0.3, 
       lab=c('FP','FS','DI','PI','EI','LUM','EC'), 
       pos=1, padj=0)
}


# plot and save to pdf
pdf_plot = function(df,alpha,finger=FALSE, outputdir, filename) {
  # start pdf
  #pdf(file=paste(outputdir, filename, sep="/"), height=2.3, width=4,compress=FALSE)
  jpeg(file=paste(outputdir, filename, sep="/"), width=600, height=600, quality=1, res=300)
  # plot lines
  par(mar=c(0,0,0,0), mgp=c(0,0,0))
  add_paracoord_points(df,alpha)
  # plot axes
  create_axes(df, finger)
  # flush out pdf
  dev.off()
}

# pdf_mplot
pdf_mplot = function(df,alpha,finger=FALSE) {
  # start pdf
  pdf(file="Downloads/mult_figure.pdf", height=3, width=4,compress=FALSE)
  # make multiple subplots
  par(mar=c(0,0.5,0,0.5), mgp=c(0,0,0),mfcol = c(3,2))
  
  N = dim(df)[1]
  
  # 1 PI < 60%
  temp = df[df$`4` < 0.6,]
  points(temp,alpha)
  axes(df)
  label_axes(df,finger)
  
  # 2 DI < 60%
  temp = df[df$`3` < 0.6,]
  points(temp,alpha)
  axes(df)
  
  # 3 PI < 60% and DI < 60%
  temp = df[(df$`3` < 0.6) & (df$`4` < 0.6),]
  points(temp,alpha)
  axes(df)
  
  # 4 lower 50% of L1
  temp = df[df$`8` %in% sort(df$`8`)[1:floor(N/2)],]
  points(temp,alpha)
  axes(df)
  label_axes(df,finger)
  
  # 5 lower 50% of L2w
  temp = df[df$`12` %in% sort(df$`12`)[1:floor(N/2)],]
  points(temp,alpha)
  axes(df)
  
  # 6 lower 50% of all costs
  temp = df
  temp[,20] = rowSums(df[,14:19])
  temp = temp[temp[,20] %in% sort(temp[,20])[1:floor(N/2)],]
  temp = temp[,1:19]
  points(temp,alpha)
  axes(df)
  
  # flush out pdf
  dev.off()
}



# make_plot
# in: csv file (file), (fmax), number of points (N), transparency (t)
# out: pdf of plot in Downloads folder
paracoord_plot = function(file, fmax, N, t, finger=FALSE, outputdir, outputname=basename(file_path_sans_ext(file))) {
  df = read.csv(file)
  #df = make_dataframe(data[1:N,])
  #df = fill_costs(df,fmax)
  #df = fill_axes(df)
  pdf_plot(df,t,finger, outputdir=outputdir, filename=,paste(outputname,".jpg", sep=""))
}



# make_mplot
make_mplot = function(file, fmax, N, t, finger=FALSE) {
  data = read.csv(file)
  df = make_dataframe(data[1:N,])
  df = fill_costs(df,fmax)
  df = fill_axes(df)
  pdf_mplot(df,t,finger)
}

