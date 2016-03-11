library(rgl)

forceplot_overlay <- function(a, ...){
  time <- 1:length(a$Force.x)
  plot(time, a$Force.x, type='l', ...)
  lines(time, a$Force.y, col='red')
  lines(time, a$Force.z, col='blue')
  lines(time, a$Mx, col='green')
  lines(time, a$M.y, col='cyan')
  lines(time, a$M.z, col='yellow')
}

palmar <- read.csv('palmar_1330pst.txt', header=TRUE)
off_palmar <- read.csv('offPalmar_1344pst.txt', header=TRUE)
#Francisco did 3 separate off palmar forces into the JR3, all were recorded in one shot. This was done to make sure that the off palmar forcewasn't too big. For francisco to stay in the same spot he was also pushing a little sideways. he was shooting for 22 degrees, but in reality he got 7ish degrees from palmar, because there's a bit of force placed laterally (that he couldn't control.) there's a geometric argument. 
multiple_off_palmar <- read.csv('3offPalmar_1439pst.txt', header=TRUE)

par(mfrow=c(2,3))
forceplot_overlay(palmar, main="Palmar Force", ylim=c(-0.1,0.651))
forceplot_overlay(off_palmar, main = "Off-Palmar Force",ylim=c(-0.1,0.651))
forceplot_overlay(multiple_off_palmar, main="3 off palmar forces", ylim=c(-1,1))
#this is just the sanity check. not used in futher experiments

#Extract the parts that we manually labeled ourselves as the 'static hold part'


hold_from_null_force<- palmar[100:400,]
hold_from_palmar_force <- palmar[1000:1300,]
hold_from_off_palmar_force <- off_palmar[1300:1600,]

forceplot_overlay(hold_from_palmar_force, main="Palmar Force (snippet used for calibration)", ylim=c(-1,1))
forceplot_overlay(hold_from_off_palmar_force, main = "Off-Palmar Force (snippet used for calibration)",ylim=c(-1,1))
forceplot_overlay(hold_from_null_force, main = "Null Force (snippet used for calibration)",ylim=c(-1,1))

null_mean_vector <- colMeans(hold_from_null_force)
palmar_mean_vector <- colMeans(hold_from_palmar_force)
off_palmar_mean_vector <- colMeans(hold_from_off_palmar_force)


print("null Means and summary")
print(null_mean_vector)
print(summary(hold_from_null_force))

print("Palmar Means and summary")
print(palmar_mean_vector)
print(summary(hold_from_palmar_force))

print("Off Palmar Means and summary")
print(off_palmar_mean_vector)
print(summary(hold_from_off_palmar_force))

# TODO port kian's matlab plot into R



