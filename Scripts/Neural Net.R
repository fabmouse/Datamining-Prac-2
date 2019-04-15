###################
#NEURAL NET SCRIPT#
###################

library(neuralnet)
library(ggplot2)
source("Scripts/Functions.R")

# AIM ONE ----------------------------------------------------------------------
data.vote <- read.csv("data/Clean Vote Data.csv", header = TRUE)
head(data.vote)

nn_boot <- myboot(B = 3, model = "Neural Net")

plot <- ggplot(nn_boot$Acc_All, aes())


nn.pred <- prediction(party_predictions, data.test$Party)
pref <- performance(nn.pred, "tpr", "fpr")
plot(pref)

#This fits a multinomial log loinear model via neural nets
mn.net <- nnet::multinom(as.factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                           Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train, 
                         model = TRUE)
party.predictions <- predict(mn.net, newdata=data.test, type="prob")
multiclass.roc(data.test$Party, party.predictions)

set.seed(123)
ind <- sample(1:nrow(data.vote), 0.6*nrow(data.vote), replace = FALSE)
data.train <- data.vote[ind, ]
data.test <- data.vote[-ind, ]

# estimate a neural network with one hidden layer of 8 nodes
nn <- neuralnet(as.factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                  Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.train,
                linear.output = FALSE, hidden = 1, lifesign="full")
plot(nn)

predict_testNN <- compute(nn, data.test[, c(4:11)])

#Seperate into 4 parties
party_predictions <- vector()

for(i in 1:nrow(predict_testNN$net.result)){
  result_row <- predict_testNN$net.result[i, ]
  max_ind <- which.max(result_row)
  if(max_ind == 1) party_predictions[i] = "Conservative"
  if(max_ind == 2) party_predictions[i] = "Labour"
  if(max_ind == 3) party_predictions[i] = "Other"
  if(max_ind == 4) party_predictions[i] = "Scottish National Party"
}

conf_mat <- table(data.test$Party, party_predictions)
accuracy <- sum(diag(conf_mat))/(sum(conf_mat))


# AIM TWO -----------------------------------------------------------------
data.vote <- read.csv("data/Final Vote Data.csv", header = TRUE)
data.vote <- na.omit(data.vote)
head(data.vote)

set.seed(123)
ind <- sample(1:nrow(data.vote), 0.75 * nrow(data.vote), replace = FALSE) 
data.train <- data.vote[ind, -c(1, 2, 4)]
data.validation <- data.vote[-ind, -c(1, 2, 4)] 

#Set up the cross validation to select the optimal parameters
set.seed(123)
fitControl <- trainControl(method = "cv", number = 5)

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


# AIM TWO WITH PARTY INCLUDED ---------------------------------------------
data.vote <- read.csv("data/Final Vote Data.csv", header = TRUE)
data.vote <- na.omit(data.vote)
head(data.vote)

set.seed(123)
ind <- sample(1:nrow(data.vote), 0.75 * nrow(data.vote), replace = FALSE) 
data.train <- data.vote[ind, -c(2, 4)]
data.validation <- data.vote[-ind, -c(2, 4)] 

#Set up the cross validation to select the optimal parameters
set.seed(123)
fitControl <- trainControl(method = "cv", number = 5)

nn_params <- train(factor(LeaveRemain) ~ . - Constituency, data = data.train, 
                   method = "nnet",
                   trControl = fitControl)
nn_hidden <- nn_params$bestTune$size #Suggests 1 hidden layer

nn_model <- neuralnet(factor(LeaveRemain) ~ . -Constituency, data = data.train,
                      linear.output = FALSE, hidden = nn_hidden, lifesign = "full")

plot(nn_model)

#Make predictions
#In terms of the original dataset the percentage was recorded in terms of the 
#percentage who voted to remain
#This corresponds with the second column of the nn results
predict_testNN <- compute(nn_model, data.validation[, -c(1, 2)])
nn_percent_preds <- predict_testNN$net.result[, 2]
nn_mse <- sum((nn_percent_preds - data.validation[, 3])^2)/nrow(data.validation)

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

# WORKING -----------------------------------------------------------------



# WORKING -----------------------------------------------------------------


