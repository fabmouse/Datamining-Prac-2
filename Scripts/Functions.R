myboot <- function(B, model, ROC = FALSE){
  set.seed(123)
  accuracy <- vector() #vector to store the accuracy
  
  if(model == "Naive Bayes"){
    for(j in 1:B){
      bs <- sample(1:nrow(data.vote), nrow(data.vote), replace = T) ## bootstrap
      vote.train <- data.vote[bs, -c(1,3)] # training dataset
      vote.test <- data.vote[-bs, -c(1,3)] # test dataset
      
      #Set up vectors
      predclass <- vector() #vector to store predictions
      #TPR <- vector() #vector to store sensitivity
      #FPR <- vector() #vector to store 1-specificity
      
      #Fit a Naive Bayes
      fitmodel <- naiveBayes(factor(Party) ~ ., data = vote.train)
      
      #Calculate predictions
      predclass <- predict(fitmodel, newdata=vote.test[,-1]) ## randomised predictions
      
      #Confusion Matrix and calculate accuracy
      conf_mat <- table(predclass, vote.test$Party)
      accuracy[j] <- sum(diag(conf_mat))/(sum(conf_mat))
    }
  } else if (model == "Neural Net")
    for(j in 1:B){
      bs <- sample(1:nrow(data.vote), nrow(data.vote), replace = T) ## bootstrap
      data.train <- data.vote[bs, -c(1,3)] # training dataset
      data.test <- data.vote[-bs, -c(1,3)] # test dataset
      
      #Set up vectors
      predclass <- vector() #vector to store predictions
      #TPR <- vector() #vector to store sensitivity
      #FPR <- vector() #vector to store 1-specificity
  
      #Fit the neural net
      fitmodel <- neuralnet(as.factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                              Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train,
                            linear.output = FALSE, hidden = 1, lifesign="full")
      
      #Calculate predictions
      predict_testNN <- compute(nn, data.test[, -1])
      for(i in 1:nrow(predict_testNN$net.result)){
        result_row <- predict_testNN$net.result[i, ]
        max_ind <- which.max(result_row)
        if(max_ind == 1) predclass[i] = "Conservative"
        if(max_ind == 2) predclass[i] = "Labour"
        if(max_ind == 3) predclass[i] = "Other"
        if(max_ind == 4) predclass[i] = "Scottish National Party"
      }
      
      #Confusion Matrix and calculate accuracy
      conf_mat <- table(data.test$Party, predclass)
      accuracy[j] <- sum(diag(conf_mat))/(sum(conf_mat))
    }
  
  #Boostrap accuracy
  bootstrap_acc <- mean(accuracy)              
  return(list(Acc_All = accuracy,
              Acc_Overall = bootstrap_acc))
}
