source("GradientDescentHelper.R")
#Function Name: separateData
#Algorithm: This will take in a data matrix, and then partition it into k
#           sets randomly (row wise)
#Inputs:
#data: Data matrix, doesnt really matter what it represents
#k: Number of sets to split our data into
#Outputs:
#data_folds_list: data_folds_list is in the form of a list, every element is a matrix,
#                 where that matrix is is a subsection of data. data_folds_list is
#                 a partition of the data in the form of a list (row wise)
separateData <- function( data, k )
{
  #initialization
  num_rows <- nrow( data )
  data_folds_list <- list()
  
  #shuffle data (row wise)
  data <- data[sample.int(num_rows), ]
  
  #split num_rows into k parts using split and store in rows_indices list
  indices_list <- split( 1:num_rows, 1:k )
  #will give a warning if(num_rows %% k != 0)
  
  #assign data to each fold
  for( fold in 1:k )
  {
    #split the data into k folds
    data_folds_list[[ fold ]] = data[indices_list[[fold]],]
  }
  
  #return
  data_folds_list
  
}

#Function Name: ComputePredictions
#Algorithm: computers computes a yhat given an X_train, y_train, and X_new
#Inputs:
#X_train: observation data matrix used for the learning algorithm
#y_train: output vector used for the learning algorithm
#X_new: validation data matrix, never seen before
#Outputs
#pred_new: predicted y_new from the X_new data that our model predicts
ComputePredictions <- function( X_train, y_train, X_new )
{
  #train our model
  weightMatrix <- GradientDescent( X_train, y_train, stepSize, maxIterations )
  
  #predict new yhat with p value of 0.5
  pred_new <- round(sigmoid( X_new %*% weightMatrix[,maxIterations]) )
  
  #return
  pred_new
}

#Function Name: calculateError
#Algorithm: Given a yhat and y, calculate the percent error
#Inputs:
#pred_new: our yhat that some model predicts
#y_new: accurate y values, we use this to test for accuracy
#Outputs:
#error: proportion of error of predicted labels
calculateError <- function( pred_new, y_new )
{
  #calculate correctly predicted labels
  num_correct <- length(y_new[y_new==pred_new])
  #calculate total labels
  num_total <- length(y_new)
  
  #calculate accuracy and error
  accuracy <- num_correct / num_total
  error <- 1 - accuracy
  
  #return
  error
}