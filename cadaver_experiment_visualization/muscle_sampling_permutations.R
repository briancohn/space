# @param num_muscles an integer between 0 and 1
# @value grid DataFrmae containing all of the permutations across all muscle values
permutation_grid <- function(num_muscles) {
  x <- list(0:1)
  tmp = expand.grid(rep(x, num_muscles))
  return(tmp)
}

gen_trials_csv_with_number_note <- function(filename,permutation_grid, number_of_trials){
  write.csv(permutation_grid, file=filename, row.names=FALSE)
  insert_block(filename, 1, paste(number_of_trials)) 
}

main <- function(num_muscles=7,filename) {
  grid<- permutation_grid(num_muscles) 
  gridlen <- length(grid[,1])
  gen_trials_csv_with_number_note("seven_muscles.csv",grid,gridlen)
}
insert_block <- function(textfilename, line_after_which_we_will_paste, block){
  # text will be inserted after the line
  idx <- line_after_which_we_will_paste 
  # open the file and read in all the lines 
  conn <- file(textfilename)
  text <- readLines(conn)
  text_block <- unlist(strsplit(block, split='\n'))
  # concatenate the old file with the new text
  mytext <- c(text[1:idx],text_block,text[(idx+1):length(text)]) 
  writeLines(mytext, conn, sep="\n")
  close(conn)

}


main(num_muscles=7, 'permutation_output.csv')
