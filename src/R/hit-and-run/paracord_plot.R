# input: dataframe with columns corresponding to muscles
require('ggplot2')
require('ggparallel')

paracord_plot = function(df, alpha) {

  # m = number of columns
  m <- dim(df)[2]

  color_transparent <- adjustcolor(col='blue', alpha.f = alpha)

  # plot each row of data as a line on the paracoordinate plot

  ggparallel(colnames(df), df, alpha=alpha)

}


