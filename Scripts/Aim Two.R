#######################################
#          SCRIPT FOR AIM TWO         #
#######################################

# LIBRARIES ---------------------------------------------------------------
library(caret)        #For training with caret package
library(caret)        #For the confusion matrix
library(tree)         #For the classification tree
library(ISLR)         #For the classification tree
library(rpart)        #For the classification tree
library(rpart.plot)   #For the classification tree
library(e1071)        #For the naive bayes
library(neuralnet)    #For the neural net

# Set up the datasets -----------------------------------------------------
data.vote <- read.csv("data/Final Vote Data.csv", header = TRUE)
data.vote <- na.omit(data.vote)
head(data.vote)

#Split dataset into a training set (75%) and hold back (25%) for validation
set.seed(123)
ind <- sample(1:nrow(data.vote), 0.75 * nrow(data.vote), replace = FALSE) 
data.train <- data.vote[ind, -c(1, 2, 4)]
data.validation <- data.vote[-ind,  -c(1, 2, 4)] 

#Split training dataset into a training set (75%) and testing set (25%)
set.seed(123)
tuneind <- sample(1:nrow(data.train), 0.75 * nrow(data.train), replace = FALSE) 
tune.train <- data.train[tuneind, ]
tune.test <- data.train[-tuneind, ]

#Set up the cross validation to select the optimal parameters
set.seed(123)
fitControl <- trainControl(method = "cv", number = 5)

# CLASSIFICATION TREE -----------------------------------------------------


# DECISION TREE -----------------------------------------------------------


# RANDOM FOREST -----------------------------------------------------------


# NAIVE BAYES -------------------------------------------------------------

# SUPPORT VECTOR MACHINE --------------------------------------------------

# NEURAL NET --------------------------------------------------------------
nn_params <- train(factor(LeaveRemain) ~ . - Constituency, data = data.train, 
                   method = "nnet",
                   trControl = fitControl)
nn_hidden <- nn_params$bestTune$size #Suggests 1 hidden layer

nn_model <- neuralnet(factor(LeaveRemain) ~ . - Constituency, data = data.train,
                      linear.output = FALSE, hidden = nn_hidden, lifesign = "full")

plot(nn_model)

#Make predictions
#In terms of the original dataset the percentage was recorded in terms of the 
#percentage who voted to remain
#This corresponds with the second column of the nn results
predict_testNN <- compute(nn_model, data.validation[, -c(1, 2)])
nn_percent_preds <- predict_testNN$net.result[, 2]
nn_mse <- sum((nn_percent_preds - data.validation[, 2])^2)/nrow(data.validation)

#Change probabilities into classes
predclass <- vector()
for(i in 1:nrow(predict_testNN$net.result)){
  result_row <- predict_testNN$net.result[i, ]
  max_ind <- which.max(result_row)
  if(max_ind == 1) predclass[i] = "Leave"
  if(max_ind == 2) predclass[i] = "Remain"
}
predclass <- as.factor(predclass)

nn_fit_metrics <- vector("list", length(levels(data.validation$LeaveRemain)))
for (i in seq_along(nn_fit_metrics)) {
  positive.class <- levels(data.validation$LeaveRemain)[i]
  nn_fit_metrics[[i]] <- confusionMatrix(predclass, data.validation$LeaveRemain,
                                         positive = positive.class)
}

nn_kappa <- nn_fit_metrics[[1]]$overall["Kappa"]
nn_acc <- nn_fit_metrics[[1]]$overall["Accuracy"]
nn_Sens <- nn_fit_metrics[[1]]$byClass["Sensitivity"]
nn_Spec <- nn_fit_metrics[[1]]$byClass["Specificity"]

# COMPARING MODEL ACCURACY ----------------------------------------
