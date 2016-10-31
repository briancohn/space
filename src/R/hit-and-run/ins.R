# input variables
require(plyr)

# file location
loc = 'Documents/USC/toy_example_recursive0.0N_positive1466442139022.csv'
loc2 = 'Documents/USC/space/output/toy_example_recursive14.405619707694639N_positive1467508047038.csv'
loc3 = 'Documents/USC/space/output/toy_example_recursive14.405619707694639N_positive1467510974674.csv'

toy = 'Documents/USC/space/output/toy_example_recursive0.0N_positive1467743434668.csv'

# JR matrix (not needed)
data= c(-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669,
        -0.04689, -0.1496, 1.456*10^-17, 0.0248, 0.052, 0.0248, 0.052,
        0.06472, 0.001953, 0.0568, 0.2067, -0.1518, 0.2919, -0.1518,
        0.003081, -0.002352, 0.0001578, -0.000685, -0.0001649, -0.0004483, -0.0001649)
JR = matrix(data,4,7,byrow = TRUE)

# fmax
fmax=c(123, 219, 124.8, 129.6, 23.52, 21.6, 91.74)

# produce all gifs
files <- list.files(path="~/dev/github/space/output/", pattern="*.csv", full.names=T, recursive=FALSE)
lapply(files, function(file) {
  make_plot(file, fmax, 9, 0.7, outputdir="~/dev/github/space/src/R/hit-and-run/plots/")
})

# now for toy arm
data= c(-0.08941, -0.0447, 0.2087, -0.2138, -0.009249, 0.1421, 0.03669,
        -0.04689, -0.1496)
JR= matrix(data,3,3,byrow=TRUE)
fmax= c(10,10,10)
