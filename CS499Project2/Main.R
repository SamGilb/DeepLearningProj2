

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
  
  X_new <- X_mat[ test_fold_vec == 1, ]
  y_new <- y_vec[ test_fold_vec == 1 ]
  
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
  neighbors <- 1:20
  validation.numneighbors<-ggplot()+
    labs(x="Number of Neighbors", y="Error Percent", color = "Error", size = "Point")+ 
    geom_line(aes(x=neighbors, y=mean_error_vec,color ="Mean Error"), size=2)+
    geom_point(aes(x=which.min(mean_error_vec),y=min(mean_error_vec), size = "Min Mean Error" ),color = "red")+
    geom_line(aes(x=neighbors, y = error_mat[1,],color ="First Fold Error"))+
    geom_line(aes(x=neighbors, y = error_mat[2,],color ="Second Fold Error"))+
    geom_line(aes(x=neighbors, y = error_mat[3,],color ="Third Fold Error"))+
    geom_line(aes(x=neighbors, y = error_mat[4,],color ="Fourth Fold Error"))+
    geom_line(aes(x=neighbors, y = error_mat[5,],color ="Fifth Fold Error"))
    #+#concludes project min
    #geom_line(aes(x=neighbors, y=percent.error, data=error_mat, group=fold))# need to create table from error_mat use as data
  #alternative extract a vector and graph for each fold
    
  print(validation.numneighbors)
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
                                                  NearestNeighborsCV( X_train, y_train, X_new )[[1]]
                                                },
                           test_fold_vec )
  

  
  #calc error using knn with k = 1
  knn_error_vec <- KFoldCV( X_mat,
                            y_vec,
                            ComputePredictions = function( X_train, y_vec, X_new )
                                                 {
                                                   #calculate and pred_new using knn
                                                   knn( X_train, X_new, y_vec )
                                                 },
                            test_fold_vec
                            )
 

 test<-rbind(baseline_error_vec,NN_error_vec,knn_error_vec)

  
#create respective dotplot
dot.plot<-ggplot()+
  labs(x="test error", y="Algorithm",color="Algorithm")+
  scale_y_discrete(name ="Algorithm", breaks=seq(1,2,3), limits= c(0,5))+
  geom_point(aes(x=test[1,1:4], y=1,color="Baseline"))+
  geom_point(aes(x=test[2,1:4], y=2,color="NearestNeighbors"))+
  geom_point(aes(x=test[3,1:4], y=3,color="OneNearestNeighbors"))

  
#display
print(dot.plot)




  #create respective dotplot
  baseline.dot.plot<-ggplot()+
    geom_point(aes(x=1:4, y=baseline_error_vec))
  #display
  print(baseline.dot.plot)
  
  #create respective dotplot
  NN.dot.plot<-ggplot()+
    geom_point(aes(x=1:4, y=NN_error_vec))
  
  #display
  print(NN.dot.plot)
  
  #create respective dotplot
  knn.dot.plot<-ggplot()+
    geom_point(aes(x=1:4, y=knn_error_vec)) 

 
  #display
  print(knn.dot.plot)
}

main()