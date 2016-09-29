# make dataframe from standard paracoord database file
# removes the first 2 lines (manually input 0's and 1's)
DataFrame = function(file) {
  df = read.csv(file,header=TRUE)
  n = dim(df)[1]
  result = df[3:n,]
  rownames(result) = 1:(n-2)
  return(result)
}

# find number of muscles
num_muscles = function(df) {
  N = match("fx",colnames(df))
  return(N-1)
}

# return matrix of unique tasks
# default now is in order of appearance (for sample csv: small to large)
task_df = function(df) {
  N = num_muscles(df)
  result = unique(df[,(N+1):(N+4)])
  rownames(result) = 1:dim(result)[1]
  return(result)
}

# find number of tasks
num_tasks = function(df){
  tasks = task_df(df)
  return(dim(tasks)[1])
}

# asign task numbers for each row of activations
# inputs: original df, unique task matrix (in the order you want them)
assign_tasks = function(df, tasks) {
  df['task'] = 0
  N = num_tasks(df)
  for (i in 1:N){
    df[df$fx==tasks[i,]$fx & df$fy==tasks[i,]$fy & df$fz==tasks[i,]$fz & df$tx==tasks[i,]$tx,]$task = i
  }
  return(df)
}

# make adjusted dataframe directly from file
adjusted_dataframe = function(file){
  df = DataFrame(file)
  tasks = task_df(df)
  newdf = assign_tasks(df,tasks)
  return(newdf)
}

# make histogram for 1 task
histogram = function(newdf,task,muscle,bins,xmin=0,xmax=1) {
  arr = newdf[newdf$task == task,muscle]
  breaks = seq(xmin,xmax,length=bins+1)
  return(hist(arr,breaks=breaks,plot=FALSE)$count)
}

# make counts for every task 
# flip so that first task is on top (as the highest y)
counts = function(newdf,muscle,bins,xmin=0,xmax=1){
  N = num_tasks(newdf)
  result = mapply(histogram, task=1:N,
                  MoreArgs=list(newdf=newdf,muscle=muscle,bins=bins,xmin=xmin,xmax=xmax))
  return(result[,N:1])
}

# check for valid muscle input
valid_muscle = function(df, muscle){
  N = num_muscles(df)
  if (is.na(match(muscle,1:N))) {
    print(paste("Error: not a valid muscle. Number of muscles =", as.character(N)))
    return(FALSE)
    }
  else {
    return(TRUE)
  }
}

# return min max for 1 task
minmax = function(newdf,task,muscle) {
  lower = min(newdf[newdf$task==task,muscle])
  upper = max(newdf[newdf$task==task,muscle])
  return(c(lower,upper))
}

# return min max matrix for every task for muscle
# fip to correspond with counts matrix
minmax_matrix = function(newdf,muscle){
  N = num_tasks(newdf)
  result = mapply(minmax,task=1:N,MoreArgs=list(newdf=newdf,muscle=muscle))
  return(result[,N:1])
}

# add lines for min/max
add_minmax = function(newdf,muscle){
  N = num_tasks(newdf)
  mat = minmax_matrix(newdf,muscle)
  height = 1
  for (i in 1:N){
    low = mat[1,i]
    y0 = i - (height/2)
    high = mat[2,i]
    y1 = i + (height/2)
    segments(low,y0,low,y1,lty="dashed",lwd=2,col="limegreen")
    segments(high,y0,high,y1,lty="dashed",lwd=2,col="limegreen")
  }
}

# add line for maximum output
add_maximal = function(maximal,muscle){
  height = 1
  x = maximal[muscle]
  abline(v=x,col="red",lwd=2)
}

# plot axes
# NOTE: labels are from top to bottom
axes = function(freq,xmin=0,xmax=1,labels=c()){
  # x axis
  axis(1, at=seq(xmin,xmax,length=5), labels=seq(xmin,xmax,length=5), lwd=0, lwd.ticks=0.5)
  mtext("activation",side=1,line=3)
  
  # y axis
  # flip labels so task 1 is on top
  if (length(labels) == 0){
    labels = dim(freq)[2]:1
  }
  else{
    labels = labels[length(labels):1]
  }
  # axis(4, at=1:7, labels=labels, lwd=0, lwd.ticks=0.5, las=2)
  mtext("task",side=4, line=3)
}

# plot map
# NOTE: labels are from top to bottom
plotmap = function(freq,xmin=0,xmax=1){
  # margins
  par(mai=c(1.02,0.82,0.82,1.02))
  
  # colors
  #r = colorRampPalette(c("gray88","blue"))(1000)
  r = colorRampPalette(c("gray88","yellow","orange","red"))(1000)
  
  # plot
  N = dim(freq)[2] # num tasks
  image(x=seq(xmin,xmax,length=dim(freq)[1]+1),
        y=1:N,z=freq,col=r,axes=FALSE,xlab="",ylab="")
  box()
}

# input: histogram with task numbers, specify muscle, number of bins
# NOTE: labels are from top to bottom
vectormap = function(file, muscle, bins, maximal, xmin=0, xmax=1,labels=c()) {
  # adjusted df with tasks
  newdf = adjusted_dataframe(file)
  
  # check for valid muscle number
  if (!valid_muscle(df,muscle)) return()

  # get counts
  freq = counts(newdf,muscle,bins, xmin, xmax)
  
  # pdf plot
  filename=paste(c("Downloads/Figures/figure_",as.character(muscle),".pdf"),collapse="")
  pdf(file=filename, height=7, width=7,compress=FALSE)
  
  plotmap(freq,xmin,xmax)
  axes(freq,xmin,xmax,labels=labels)
  title(toupper(colnames(df)[muscle]))
  add_minmax(newdf,muscle)
  add_maximal(maximal,muscle)
  
  dev.off()
}

# ###################
# ##     main      ##
# ###################
# sample = 'Documents/USC/space/output/tests/sample.csv'
# maxval = c(0.20443228678688036, 0.13803648681868025, 1.0000000000000004, 0.0, 0.32321402147358713, 1.0, 1.0)

# # try for all muscles in finger
# for (i in 1:7){
#   vectormap(sample,i,20,labels=seq(0,0.9,length=10),maximal=maxval)
# }

# vectormap(sample,7,20,maxval)
# vectormap(sample,1,20,maxval,labels=seq(0,0.9,length=10))
# vectormap(sample,9,20,maxval)
# vectormap(sample,2,20,maxval,labels=c('f','s','t','f','f','s','s','e','n','t'))
