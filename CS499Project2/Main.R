

main <- function()
{
  #initialization
  source("KFoldCV&NearestNeighborsCV.R")
  library("data.table")
  set.seed(8)
  
  #############################################################################
  #                         Data Upload and Organization                      #
  #############################################################################
  #import_data
  import_data <- fread("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data")
  import_data <- matrix( unlist(import_data), 4601, 58)
  
  #categorize data into X_mat and y_vec
  X_mat <- import_data[,1:57]
  X_mat <- scale( X_mat )
  y_vec <- import_data[,58]
  
  #make test_fold_vec for later use
  test_fold_vec <- sample( rep( 1:4, l = length(y_vec) ) )
  
  #############################################################################
  #                         Plotting line graph(S)                            #
  #############################################################################
  
  #store the return for nearest neighbor for data plotting
  NN_list <- NearestNeighborsCV( X_mat, y_vec, X_new )
  
  #separate return statement into separate variables
  pred_new <- NN_list[[1]]
  mean_error_vec <- NN_list[[2]]
  error_mat <- NN_list[[3]]
  
  #plot neighbors(1:20) on x and mean_error_vec * 100 on y axis in line graph format (bold)
  #plot (which.min(mean_error_vec), min(mean_error_vec))
  #make sure labels are good
  #put in legend
  
  #############################################################################
  #                              Table for PDF                                #
  #############################################################################
  
  #separate labels into folds to create table later
  first_fold_labels <- y_vec[test_fold_vec == 1]
  second_fold_labels <- y_vec[test_fold_vec == 2]
  third_fold_labels <- y_vec[test_fold_vec == 3]
  fourth_fold_labels <- y_vec[test_fold_vec == 4]
  
  #create data for table
  table_data <- c( table(first_fold_labels)[[1]],
                   table(first_fold_labels)[[2]],
                   table(second_fold_labels)[[1]],
                   table(second_fold_labels)[[2]],
                   table(third_fold_labels)[[1]],
                   table(third_fold_labels)[[2]],
                   table(fourth_fold_labels)[[1]],
                   table(fourth_fold_labels)[[2]]
                   )
  #create table matrix
  table <- matrix( table_data, 4, 2, byrow = TRUE )
  #edit table names
  colnames(table) <- c( "0", "1" )
  rownames(table) <- c( "1", "2", "3", "4" )
  table <- as.table(table)
  #print table for pdf
  print(table)
  
  
  #############################################################################
  #                    ComputePrediction Functions Testing                    #
  #############################################################################
  
  #calc baseline errorvec with baseline prediction function
  baseline_error_vec <- KFoldCV( X_mat,
                                 y_vec,
                                 ComputePredictions = function( X_train, y_train, X_new )
                                                      {
                                                        #calculate pred_new
                                                        pred_new <- replicate( nrow(X_new), 0 )
                                              
                                                        #return
                                                        pred_new
                                                      },
                                 test_fold_vec )
  
  #calc error using NN algorithm
  NN_error_vec <- KFoldCV( X_mat,
                           y_vec,
                           ComputePredictions = function( X_train, y_train, X_new )
                                                {
                                                  #calculate and access pred_new
                                                  NearestNeighborsCV( X_train, y_vec, X_new )[[1]]
                                                },
                           test_fold_vec )
  
  #calc error using knn with k = 1
  knn_error_vec <- KFoldCV( X_mat,
                            y_vec,
                            ComputePredictions = function( X_train, y_train, X_new )
                                                 {
                                                   #calculate and pred_new using knn
                                                   knn( X_train, X_new, y_vec )
                                                 },
                            test_fold_vec
                            )
  
  #plot this stuff!!!!!!
  
  
  
  
  
  
  
  #lol wtf
  #X_mat <- import_data[,1:(ncol(import_data) - 1)]
  
  #THIS LINE DOES NOT WORK DO NOT USE THIS LINE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #DONT DO IT MAN
  #ask hocking why this doesnt work
  #y_vec <- import_data[,ncol(import_data)]
  #y_vec <- import_data[,58]
}
