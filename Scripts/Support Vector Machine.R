#### support vector machines for multiple classes 

library(e1071)
data.vote <- read.csv("data/Clean Vote Data.csv", header=T)
ind <- sample(1:nrow(data.vote), 0.6 * nrow(data.vote), replace = FALSE) ## bootstrap
data.train <- data.vote[ind, ] # training dataset
data.test <- data.vote[-ind, ] # test dataset

set.seed(123)

#Set up the support vector machine
#Kernel = 
svmfit <- svm(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train,
              kernel = "linear", cost = 10, gamma = 1)

#Try a range of cost parameters
#Select the best cost parameter using cross validation 
#Using the caret package for carrying out CV and Cost selection

## 5fold CV
set.seed(123)
fitControl <- trainControl(method = "cv", number = 5)
svm_cost <- train(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                     Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train, 
                   method = "svmRadialCost",
                   trControl = fitControl)
best_cost <- svm_cost$bestTune

#Fit SVM for best cost
svmfit <- svm(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train,
              type = "C", kernel = "radial", cost = 1, gamma = 1)

summary(svmfit)

#Obtain predictions and create the confusion matrix
predclass <- predict(svmfit, data.test[, 4:11])
conf_mat <- table(observed = data.test$Party, predicted = predclass)

fit_metrics <- vector("list", length(levels(data.test$Party)))
for (i in seq_along(fit_metrics)) {
  positive.class <- levels(data.test$Party)[i]
  # in the i-th iteration, use the i-th class as the positive class
  fit_metrics[[i]] <- confusionMatrix(predclass, data.test$Party, 
                                      positive = positive.class)
}

#Accuracy
sum(diag(conf_mat))/(sum(conf_mat))

#Sensitivity and Specificity
fit_metrics[[1]]$byClass[, "Sensitivity"]
fit_metrics[[1]]$byClass[, "Specificity"]

##From Lab 4:


tunePars = tune(svm, factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                  Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train, 
                kernel ="radial",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1,5, 10, 100)))

summary(tunePars)

bestModel<- tunePars$best.model
summary (  bestModel)

svm_cost <- best_cost

##Checking bootstrap:
#for(j in 1:B){
j <- 1
  bs <- sample(1:nrow(data.vote), nrow(data.vote), replace = T) ## bootstrap
  vote.train <- data.vote[bs, -c(1,3)] # training dataset
  vote.test <- data.vote[-bs, -c(1,3)] # test dataset
  
  #Fit the svm
  fitmodel <- svm(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                    Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train,
                  type = "C", kernel = "radial", cost = svm_cost, gamma = 1)
  
  predict(svmfit, data.test[, 4:11])
  
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
#}

