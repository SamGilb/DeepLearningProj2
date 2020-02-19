source("KFoldCV&NearestNeighborsCVHelper.R")
library("class")

KFoldCV <- function( X_mat, y_vec, ComputePredictions, fold_vec )
{
  #initialization
  K <- max( fold_vec )
  error_vec <- numeric( K )
  
  for( k in 1:K )
  {
    #separate data with the k'th fold being validation
    X_new <- X_mat[ fold_vec == k, ]
    y_new <- y_vec[ fold_vec == k ]
    
    #set aside training data for this iteration
    X_train <- X_mat[ fold_vec != k, ]
    y_train <- y_vec[ fold_vec != k ]
    
    #computer predictions and store result in pred_new
    pred_new <- ComputePredictions( X_train, y_train, X_new )
    
    #calculate and store error
    error_vec[ k ] <- calculateError( pred_new, y_new )
  }
  
  #return error_vec
  error_Vec
}



NearestNeighborsCV( X_mat, y_vec, X_new, num_folds = 5, max_neighbors = 20 )
{
  #initialize validation_fold_vec for later categorizing into folds
  validation_fold_vec <- sample( rep( 1:num_folds, l = nrow(X_mat) ) )
  
  #initialize error_mat
  error_mat <- matrix( NA, num_folds, max_neighbors )
  
  for( num_neighbors in 1:max_neighbors )
  {
    #create error_vec for some num_neighbors neighbors and store
    error_vec <-KFoldCV( X_mat,
                         y_vec,
                         ComputePredictions = function( X_train, y_train, X_new )
                         {
                           knn(X_train, X_new, y_train, k = num_neighbors)
                         },
                         validation_fold_vec )
    
    #append error_vec to error_mat (column wise)
    error_mat[,num_neighbors] <- error_vec
  }
  
  #calculate mean of every column (mean of k folds error for some fixed num_neighbors)
  mean_error_vec <- colMeans( error_mat )
  
  #find the index of least mean error (best neighbors)
  best_neighbors <- which.min( mean_error_vec )
  
  #predict new labels
  pred_new <- knn( X_train, X_new, y_train, k = best_neighbors )
  
  #return
  list( pred_new, mean_error_vec, error_mat )
}


























































# source("KFoldCV&NearestNeighborsCVHelper.R")
# library("class")
# 
# #Function Name: KFoldCV
# #Algoirthm: Given a data matrix and label vector, and also a function to 
# #           predict weights in a general additive model, and fold_vec
# #           indicating how many folds to use, this function will separate
# #           the data into K folds and train a model using each respective
# #           training fold. Then, it will compute the error for each fold
# #           and output them as a vector
# #Inputs:
# #X_mat: data matrix, rows = observations, columns = features
# #y_vec: label vector corresponding to X_mat
# #ComputePredictions: function that takes in inputs X_train, y_train, and X_new
# #                    ComputePredictions will predict yhat, a list of predicted
# #                    labels. 
# #fold_vec: vector containing integers 1:K, where K is the number of folds
# #Outputs:
# #error_vec: numeric vector where error_vec[k] represents the k'th fold being
# #           the validation fold
# KFoldCV <- function( X_mat, y_vec, ComputePredictions, fold_vec )
# {
#   #initialization
#   k = max( fold_vec )
#   error_vec = numeric( k )
#   
#   #group y and X data for easier implementation
#   #note that the first column of data is y_vec
#   #note that the rest of data is X_mat
#   data <- cbind( y_vec, X_mat )
#   
#   #separate data into k folds
#   data_folds_list <- separateData( data, k )
#   
#   #iterate through every possible k
#   for( test_fold in fold_vec )
#   {
#     #isolate test data
#     data_new_matrix <- data_folds_list[[ test_fold ]]
#     #extract validation data from test_fold
#     X_new <- data_new_matrix[,-1]
#     y_new <- data_new_matrix[,1]
#     
#     #extract train data from test_fold
#     data_train_list <- data_folds_list[ -1 * test_fold ]
#     #convert list of matrices to rbind'd matrices
#     data_train_matrix <- matrix(unlist(lapply(data_train_list, t) ), ncol = ncol(data_new_matrix), byrow = TRUE)
#     #extract training data into X and y
#     X_train <- data_train_matrix[,-1]
#     y_train <- data_train_matrix[,1]
#     
#     #computer predictions and store for later use
#     #print(c("X_train", X_train))
#     pred_new <- ComputePredictions( X_train, y_train, X_new )
#     
#     #calculate error for this fold
#     error <- calculateError( pred_new, y_new )
#     
#     #store error
#     error_vec[test_fold] <- error
#   }
#   
#   #return error_vec
#   error_vec
# }
# 
# #Function Name: NearestNeighborsCV
# NearestNeighborsCV <- function( X_mat, y_vec, X_new, num_folds = 5, max_neighbors = 20 )
# {
#   #initialization
#   validation_fold_vec <- 1:num_folds
#   error_mat <- matrix(NA, num_folds, max_neighbors )
#   
#   for( num_neighbors in 1:max_neighbors )
#   {
#     #calculate error_vec for a given num_neighbors
#     error_vec <- KFoldCV( X_mat, 
#                           y_vec, 
#                           ComputePredictions = function( X_train, y_train, X_new )
#                                                {
#                                                   knn(X_train, X_new, y_train, k = num_neighbors)
#                                                },
#                           1:num_folds )
#     
#     #append error_vec to error_mat column wise
#     error_mat[,num_neighbors] <- error_vec
#   }
#   
#   #take mean of every column
#   mean_error_vec <- colMeans(error_mat)
#   
#   #calculate best number of neighbors
#   best_neighbors <- which.max(mean_error_vec)
#   
#   #return
#   list( knn(X_train, X_new, y_train, k = best_neighbors ), mean_error_vec, error_mat )
# }