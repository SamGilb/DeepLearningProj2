source("KFoldCV&NearestNeighborsCV.R")

main <- function()
{
  #import_data
  import_data <- fread("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data")
  import_data <- matrix( unlist(import_data), 4601, 58)
  
  #categorize data into X_mat and y_vec
  X_mat <- import_data[,1:57]
  X_mat <- scale( X_mat )
  y_vec <- import_data[,58]
  
  NearestNeighborsCV( X_mat, y_vec, )
  
  #lol wtf
  #X_mat <- import_data[,1:(ncol(import_data) - 1)]
  
  #THIS LINE DOES NOT WORK DO NOT USE THIS LINE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #DONT DO IT MAN
  #ask hocking why this doesnt work
  #y_vec <- import_data[,ncol(import_data)]
  y_vec <- import_data[,58]
}
