# posterior = likelyhood * prior / marginal prob 
# marginal prob is same for all


# calculatin priors


library(matrixStats)


convert_to_num_matrix <- function(data_df)
{
  result_matric <- c()
  
  for ( i in c( 1:ncol(data_df) ) ) 
  {
    if( is.factor( data_df[ , i]) == TRUE )
    {
      result_matric <- cbind( result_matric , as.numeric(as.character(data_df[,i]) ) ) 
    }
    else
    {
      result_matric <- cbind( result_matric , as.numeric(data_df[,i]) )
    }
    
  }
  
  return( result_matric )
}






priors <- function(cl_var)
{
  
  #cl_var is vector
  tab_cont <- table(cl_var)
  
  prior_val <- matrix(tab_cont , nrow = 1)
  colnames(prior_val) <- names(tab_cont)
  prior_val <- prior_val / length(cl_var)  
  
  return(prior_val)
  
}


probability_paramters <- function(train_data , cl_var)
{
  # train data is matrix
  # cl_var is vector
  
  
  class_val <- levels(cl_var)
  
  
  result_mean <- matrix( numeric(0), nrow = 0, ncol = ncol(train_data))
  result_sd <- matrix( numeric(0), nrow = 0, ncol = ncol(train_data))

      for (i in class_val) {
        mean_mat <-  matrix(colMeans(train_data[which(cl_var == i) , ]) , nrow = 1)
        result_mean <- rbind(result_mean , mean_mat)
        
        
        sd_mat <- matrix(colSds(train_data[which(cl_var == i) , ]) , nrow = 1)
        result_sd <- rbind(result_sd , sd_mat)
        
      }
  
  colnames(result_mean) <- colnames(train_data)
  rownames(result_mean) <- class_val
  
  
  colnames(result_sd) <- colnames(train_data)
  rownames(result_sd) <- class_val
  
  return(list("Mean" = result_mean ,"SD" = result_sd))
}





naive_bayes <- function(train_data , cl_var)
{
  
  dataset <- convert_to_num_matrix(train_data)
  
  colnames(dataset) <- colnames(train_data)
  
  priors_values <- priors(cl_var)
  
  prob_params <- probability_paramters(train_data = dataset , cl_var = cl_var)
  
  
  
  return(list("Priors" = priors_values ,"Prob_Params" =  prob_params))
  
  
  
}





calculate_prob <- function(trained_model  , testing_row)
{
  
  result <- 1
  for (i in c(1: ncol(testing_row))) 
  {
   temp_res <-  dnorm(testing_row[1 , i] , trained_model$Prob_Params$Mean[ , i] , trained_model$Prob_Params$SD[ , i])
   result <- result * temp_res
  }
  
  
  print("------------------------------")
  
  result <- result * train_naive$Priors[1,]
  
  return(names(which.max(result)))
}

prediction <- function(trained_model , testing_data)
{
  
  result <- c()
  
  for (i  in c(1:nrow(testing_data))) 
  {
    row_res <- calculate_prob(trained_model = trained_model , testing_row =testing_data[i , ])
    print(i)
    print(row_res)
    result <- c(result , row_res)  
  }
  
  
  result <- as.factor(result)
  
  return(result)
  
}










require(caTools)
 
set.seed(3237)
sapmle_split <- sample.split(iris$Species , SplitRatio = 0.9)
train_data <- subset(iris , sapmle_split == TRUE)
test_data <- subset(iris  , sapmle_split == FALSE)


train_naive <- naive_bayes(train_data = iris[,-5] , cl_var = iris[,5])


predicted_result <- prediction(trained_model = train_naive , testing_data = test_data[,-5])

library(caret)

confusionMatrix(table(predicted_result , test_data[,5]))



















