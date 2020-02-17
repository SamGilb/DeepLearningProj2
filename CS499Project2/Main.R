source("KFoldCV&NearestNeighborsCV.R")

main <- function()
{
  #import_data
  import_data <- fread("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data")
  
  #categorize data into X_mat and y_vec
  X_mat <- import_data[,1:(ncol(import_data) - 1)]
  #THIS LINE DOES NOT WORK DO NOT USE THIS LINE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #DONT DO IT MAN
  y_vec <- import_data[,ncol(import_data)]
}