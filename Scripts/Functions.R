###########################################
#               FUNCTIONS                 #
###########################################
#Script containing functions used with models


# LIBRARIES --------------------------------------------------------------------
library(e1071)        #For the naive bayes
library(neuralnet)    #For the neural net
library(caret)        #For the confusion matrix

# BOOTSTRAP FUNCTION -----------------------------------------------------------
#Purpose: Carry out bootstrap validation on a selected model
#Inputs: seed - the seed used as a starting point for the random number generator
#        B - the number of bootstrap smaples to be generated
#        model - the model to be evaluated. Either "Naive Bayes" or "Neural Net"
#        ROC - a boolean indicating if a an ROC plot should be produced
#Outputs: summary of the accuracy, specificity and sensitivity
#Implimentation notes: no error checking

myboot <- function(seed, B, model, nn_hidden = 1, svm_cost = 1, ROC = FALSE){
  set.seed(seed)
  accuracy <- vector() #vector to store the accuracy
  sens_matrix <- matrix(NA, ncol = 4, nrow = B) #matrix to store sensitivity
  spec_matrix <- matrix(NA, ncol = 4, nrow = B)  #matrix to store 1-specificity
  
  if(model == "Naive Bayes"){
    for(j in 1:B){
      bs <- sample(1:nrow(data.vote), nrow(data.vote), replace = T) ## bootstrap
      vote.train <- data.vote[bs, -c(1,3)] # training dataset
      vote.test <- data.vote[-bs, -c(1,3)] # test dataset
      
      #Fit a Naive Bayes
      fitmodel <- naiveBayes(factor(Party) ~ ., data = vote.train)
      
      #Calculate predictions
      predclass <- vector()
      predclass <- predict(fitmodel, newdata = vote.test[,-1]) ## randomised predictions
      
      #Confusion Matrix and calculate accuracy
      conf_mat <- table(vote.test$Party, predclass)
      accuracy[j] <- sum(diag(conf_mat))/(sum(conf_mat))
      
      fit_metrics <- vector("list", length(levels(vote.test$Party)))
      for (i in seq_along(fit_metrics)) {
        positive.class <- levels(vote.test$Party)[i]
        # in the i-th iteration, use the i-th class as the positive class
        fit_metrics[[i]] <- confusionMatrix(predclass, vote.test$Party, 
                                   positive = positive.class)
      }
      
      sens_matrix[j, ] <- fit_metrics[[1]]$byClass[, "Sensitivity"]
      spec_matrix[j, ] <- fit_metrics[[1]]$byClass[, "Specificity"]
    }
  }else if(model == "SVM") {
    for(j in 1:B){
      bs <- sample(1:nrow(data.vote), nrow(data.vote), replace = T) ## bootstrap
      vote.train <- data.vote[bs, -c(1,3)] # training dataset
      vote.test <- data.vote[-bs, -c(1,3)] # test dataset
      
      #Fit the svm
      fitmodel <- svm(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                        Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train,
                      type = "C", kernel = "radial", cost = svm_cost, gamma = 1)
      
      #Calculate predictions
      predclass <- vector() #vector to store predictions
      predclass <- predict(fitmodel, vote.test[, -1])
      
      #Confusion Matrix and calculate accuracy
      conf_mat <- table(observed = vote.test$Party, predicted = predclass)
      accuracy[j] <- sum(diag(conf_mat))/(sum(conf_mat))
      
      fit_metrics <- vector("list", length(levels(vote.test$Party)))
      for (i in seq_along(fit_metrics)) {
        positive.class <- levels(vote.test$Party)[i]
        # in the i-th iteration, use the i-th class as the positive class
        fit_metrics[[i]] <- confusionMatrix(predclass, vote.test$Party, 
                                            positive = positive.class)
      }
      
      sens_matrix[j, ] <- fit_metrics[[1]]$byClass[, "Sensitivity"]
      spec_matrix[j, ] <- fit_metrics[[1]]$byClass[, "Specificity"]
    }
    
  } else if (model == "Neural Net") {
    for(j in 1:B){
      bs <- sample(1:nrow(data.vote), nrow(data.vote), replace = T) ## bootstrap
      vote.train <- data.vote[bs, -c(1,3)] # training dataset
      vote.test <- data.vote[-bs, -c(1,3)] # test dataset
      
      #Fit the neural net
      fitmodel <- neuralnet(as.factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                              Vote.5 + Vote.6 + Vote.7 + Vote.8, data = vote.train,
                            linear.output = FALSE, hidden = nn_hidden, lifesign = "full")
      
      #Calculate predictions
      predclass <- vector() #vector to store predictions
      predict_testNN <- compute(fitmodel, vote.test[, -1])
      for(i in 1:nrow(predict_testNN$net.result)){
        result_row <- predict_testNN$net.result[i, ]
        max_ind <- which.max(result_row)
        if(max_ind == 1) predclass[i] = "Conservative"
        if(max_ind == 2) predclass[i] = "Labour"
        if(max_ind == 3) predclass[i] = "Other"
        if(max_ind == 4) predclass[i] = "Scottish National Party"
      }
      predclass <- as.factor(predclass)
      
      #Confusion Matrix and calculate accuracy
      conf_mat <- table(vote.test$Party, predclass)
      accuracy[j] <- sum(diag(conf_mat))/(sum(conf_mat))
      
      fit_metrics <- vector("list", length(levels(vote.test$Party)))
      for (i in seq_along(fit_metrics)) {
        positive.class <- levels(vote.test$Party)[i]
        # in the i-th iteration, use the i-th class as the positive class
        fit_metrics[[i]] <- confusionMatrix(predclass, vote.test$Party, 
                                            positive = positive.class)
      }
      
      sens_matrix[j, ] <- fit_metrics[[1]]$byClass[, "Sensitivity"]
      spec_matrix[j, ] <- fit_metrics[[1]]$byClass[, "Specificity"]
    }
  }
  
  #Boostrap Metrics
  bs_Accuracy <- mean(accuracy) 
  bs_Fit <- matrix(c(colMeans(sens_matrix), colMeans(spec_matrix)), 
                   nrow = 2, byrow = TRUE)
  colnames(bs_Fit) <- levels(data.vote$Party)
  rownames(bs_Fit) <- c("Sensitivity", "Specificity")
  
  #Return accuracy, sensitivity and specificity
  return(list(BS_ACC = bs_Accuracy,
              BS_FIT = bs_Fit,
              Acc_All = accuracy))
}
