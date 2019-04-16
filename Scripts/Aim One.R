###################################################
#               FINAL AIM ONE SCRIPT              #
###################################################

# LIBRARIES ---------------------------------------------------------------
library(caret)        #For training with caret package
library(caret)        #For the confusion matrix
library(tree)         #For the classification tree
library(ISLR)         #For the classification tree
library(rpart)        #For the classification tree
library(rpart.plot)   #For the classification tree
library(e1071)        #For the naive bayes
library(neuralnet)    #For the neural net
library(dplyr)

# Set up the datasets -----------------------------------------------------
data.vote <- read.csv("data/Final Vote Data.csv", header = TRUE)
data.vote[, 6:13] <- data.vote[, 6:13] %>% mutate_if(is.numeric, as.factor)
head(data.vote)

#Split dataset into a training set (75%) and hold back (25%) for validation
set.seed(123)
ind <- sample(1:nrow(data.vote), 0.75 * nrow(data.vote), replace = FALSE) 
data.train <- data.vote[ind, -c(2:5)]
data.validation <- data.vote[-ind, -c(2:5)] 

#Split training dataset into a training set (75%) and testing set (25%)
set.seed(123)
tuneind <- sample(1:nrow(data.train), 0.75 * nrow(data.train), replace = FALSE) 
tune.train <- data.train[tuneind, ]
tune.test <- data.train[-tuneind, ]

#Set up the cross validation to select the optimal parameters
set.seed(123)
fitControl <- trainControl(method = "cv", number = 5)

# CLASSIFICATION TREE (José) ---------------------------------------------------
set.seed(123)
# Change levels into Yes, No, No Vote for nicer plots
data.vote.dt <- data.vote
for(i in 6:13){ data.vote.dt[, i]  <- recode(data.vote.dt[, i], 
                                             "1" = "Yes", "0" = "No Vote", "-1" = "No")
}

data.train.dt <- data.vote.dt[ind, -c(2, 4)]
data.validation.dt <- data.vote.dt[-ind,  -c(2, 4)] 

#Fit an initial decision tree to all of the training data
tree.voting <- tree(Party ~ ., data = data.train.dt)

#plot of decision tree
plot(tree.voting)
text(tree.voting, pretty = 0)

#Train the decision tree, using the trainging data set (338 obs)
# tree.voting <- tree(Party ~ ., data = data.train, subset = tuneind)
# tree.pred <- predict(tree.voting, tune.test, type = "class")
# 
# CMtable <- table(Observed = tune.test$Party, Predicted = tree.pred)
# accuracy.ct <- sum(diag(CMtable))/sum(CMtable)
#Accuracy = 0.9381

#Prune the tree with 5 fold cross validation
set.seed(123)
cv.voting <- cv.tree(tree.voting, FUN = prune.misclass, K = 5)
names(cv.voting)
cv.voting

#size: number of terminal nodes in each tree in the cost-complexity pruning sequence.
#deviance: total deviance of each tree in the cost-complexity pruning sequence.
#k: the value of the cost-complexity pruning parameter of each tree in the sequence.
plot(cv.voting$size, cv.voting$dev, main = "Size Results of 5 Fold CV",
     xlab = "Size", ylab = "Deviance", type = "b")
plot(cv.voting$k, cv.voting$dev, main = "Cost Complexity Results of 5 Fold CV",
     xlab = "K: Cost Complexity", ylab = "Deviance", type = "b")

#Decided that a tree with 7 terminal nodes is best
prune.voting <- prune.misclass(tree.voting, best = 7)
prune.voting

plot(prune.voting)
text(prune.voting, pretty = 0)

#Predict based on pruned tree
#Calculate accuracy
prune.pred <- predict(prune.voting, tune.test, type = "class")
CMtable <- table(Observed = tune.test$Party, Predicted = prune.pred)
prune.accuracy.ct <- sum(diag(CMtable))/sum(CMtable)

#Test on validation set
#Set up the tree on training set
#Prune to get seven temrinal nodes
tree.voting <- tree(Party ~ ., data = data.train)
prune.voting <- prune.misclass(tree.voting, best = 7)
prune.pred <- predict(prune.voting, data.validation, type = "class")

ct_fit_metrics <- vector("list", length(levels(data.validation$Party)))
for (i in seq_along(ct_fit_metrics)) {
  positive.class <- levels(data.validation$Party)[i]
  ct_fit_metrics[[i]] <- confusionMatrix(prune.pred, data.validation$Party,
                                          positive = positive.class)
}

#plots with rpart.plot#nice plots
tree.voting <- rpart(Party ~ ., data = data.train, subset = tuneind)
prune.voting <- prune(tree.voting, cp = 1)
prune.voting
prp(prune.voting, faclen = 0, cex = 0.8, extra = 1)




# RANDOM FOREST (Lei) ----------------------------------------------------------
#Lei found that a random forest with mtry = 4 and ntree = 470 works best.
library(randomForest)
source("Scripts/RFFunction.R")

# Initial Look: Fit a RF on the training set
fit.rf <- randomForest(factor(Party) ~ ., data = data.train)
print(fit.rf)

##Now improve parameters
## Step 1: Try to find a good mtry number using findgoodmtry function
n <- length(names(data.train[, -1]))
findgoodmtry(data.train[, -1], data.train[, 1], n , 123,  data.train)
## Error calculated as Out Of Bag error rate
## Both output and plot suggest mtry = 4, the error is minimum

## Step 2: try to find a good tree number
set.seed(123)
fit.rf <- randomForest(factor(Party)~ ., data = data.train, mtry = 4, 
                       ntree = 1000)
plot(fit.rf)
## when number of trees is around 300 to 400, the model is stable

## Step 3: set ntree = 350  mtry = 4, to get the basic information about these 
# paramters. 
set.seed(123)
select.rf <- randomForest(factor(Party) ~ ., data = data.train,
                          mtry = 4, ntree = 350)
print(select.rf)
## when ntree = 350, the OOB err is 3.77%

# Step 4: Try to find the good number of trees, when mtry = 4
findgoodntree(data.train[, -1],  data.train[, 1], start = 300, end = 550, by = 20,
              mtrynumber = 4, data.train)
findgoodntree(data.train[, -1],  data.train[, 1], start = 400, end = 550, by = 10,
              mtrynumber = 4, data.train)
# when mtry = 4, ntrr = 460, the accuracy = 0.9800443, OOBError = 0.04806494
# the misclass.rate = 0.01995565

#Best model 
rf_model <-  randomForest(factor(Party) ~ ., data = data.train, 
                          mtry = 4, ntree = 460)

importance(rf_model) #Shows the importance of each vote
varImpPlot(rf_model) #Shows the importance of each vote in a plot
plot(rf_model) #Shows model performance?

#Now perform validation
predclass <- predict(rf_model, newdata = data.validation[, -1]) 

rf_fit_metrics <- vector("list", length(levels(data.validation$Party)))
for (i in seq_along(rf_fit_metrics)) {
  positive.class <- levels(data.validation$Party)[i]
  # in the i-th iteration, use the i-th class as the positive class
  rf_fit_metrics[[i]] <- confusionMatrix(predclass, data.validation$Party,
                                      positive = positive.class)
}

# MULTIPLE LOGISTIC REGRESSION (Lei) -------------------------------------------
#Divide training dataset into two parts
library(nnet)

# Create initial logistic regression
## X = Voting data Y = Party, dataset = train
original.lr <- multinom(Party ~ ., data = tune.train)
summary(original.lr)
## when the X only include the voting data, the residual deviance = 153.3819 , 
#AIC = 207.3819 

#Calculate accuracy for the original logistic regression 
## Build the confusion matrix for original.lr by test dataset
prediction <- predict(original.lr, tune.test)
actuals <- tune.test$Party
CMtable <- table(Observed = actuals, Predicted = prediction)
print(CMtable)
# Accuracy and Misclass rate
accuracy.lr <- sum(diag(CMtable))/sum(CMtable)
misclass.lr <- 1 - sum(diag(CMtable))/sum(CMtable)
print(accuracy.lr) # Accuracy = 95.58%
print(misclass.lr) # Misclass rate = 4.42%

#Best model 
lr_model <- multinom(Party ~ ., data = data.train)

#Now perform validation
predclass <- predict(lr_model, newdata = data.validation[, -1]) 

lr_fit_metrics <- vector("list", length(levels(data.validation$Party)))
for (i in seq_along(lr_fit_metrics)) {
  positive.class <- levels(data.validation$Party)[i]
  # in the i-th iteration, use the i-th class as the positive class
  lr_fit_metrics[[i]] <- confusionMatrix(predclass, data.validation$Party,
                                         positive = positive.class)
}

# NAIVE BAYES (Brooke and Carlotta) --------------------------------------------
set.seed(123)
nb_params <- train(factor(Party) ~ ., data = data.train, 
                   method = "naive_bayes",
                   trControl = fitControl)
#Kernel = true
#Laplace = 0
#Adjust = 1

nb_model <- naiveBayes(factor(Party) ~ ., data = data.train)

predclass <- predict(nb_model, newdata = data.validation[, -1]) 

nb_fit_metrics <- vector("list", length(levels(data.validation$Party)))
for (i in seq_along(nb_fit_metrics)) {
  positive.class <- levels(data.validation$Party)[i]
  nb_fit_metrics[[i]] <- confusionMatrix(predclass, data.validation$Party,
                                      positive = positive.class)
}

# SUPPORT VECTOR MACHINE (Brooke and Carlotta) ---------------------------------
svm_params <- train(factor(Party) ~ ., data = data.train, 
                    method = "svmRadialCost",
                    trControl = fitControl)
svm_cost <- svm_params$bestTune$C #Suggests cost factor of 1
svm_model <- svm(factor(Party) ~ ., data = data.train,
                type = "C", kernel = "radial", cost = svm_cost, gamma = 1)

predclass <- predict(svm_model, newdata = data.validation[, -1]) 

svm_fit_metrics <- vector("list", length(levels(data.validation$Party)))
for (i in seq_along(svm_fit_metrics)) {
  positive.class <- levels(data.validation$Party)[i]
  svm_fit_metrics[[i]] <- confusionMatrix(predclass, data.validation$Party,
                                      positive = positive.class)
}

# NEURAL NET (Brooke) ----------------------------------------------------------
nn_params <- train(factor(Party) ~ ., data = data.train, 
                   method = "nnet",
                   trControl = fitControl)
nn_hidden <- nn_params$bestTune$size #Suggests 5 hidden layers

nn_model <- neuralnet(as.factor(Party) ~ ., data = data.train,
                      linear.output = FALSE, hidden = nn_hidden, lifesign = "full")
plot(nn_model)

predclass <- vector() 

#Change probabilities into classes
predict_testNN <- predict(nn_model, data.validation[, -1])
for(i in 1:nrow(predict_testNN)){
  result_row <- predict_testNN[i, ]
  max_ind <- which.max(result_row)
  if(max_ind == 1) predclass[i] = "Conservative"
  if(max_ind == 2) predclass[i] = "Labour"
  if(max_ind == 3) predclass[i] = "Other"
  if(max_ind == 4) predclass[i] = "Scottish National Party"
}
predclass <- as.factor(predclass)

nn_fit_metrics <- vector("list", length(levels(data.validation$Party)))
for (i in seq_along(nn_fit_metrics)) {
  positive.class <- levels(data.validation$Party)[i]
  nn_fit_metrics[[i]] <- confusionMatrix(predclass, data.validation$Party,
                                      positive = positive.class)
}

# PLOT COMPARING PARTY SENSITIVITY ----------------------------------------
#Obtain Kappas
ct_kappa <- ct_fit_metrics[[1]]$overall["Kappa"]
rf_kappa <- rf_fit_metrics[[1]]$overall["Kappa"]
lr_kappa <- lr_fit_metrics[[1]]$overall["Kappa"]
nb_kappa <- nb_fit_metrics[[1]]$overall["Kappa"]
svm_kappa <- svm_fit_metrics[[1]]$overall["Kappa"]
nn_kappa <- nn_fit_metrics[[1]]$overall["Kappa"]

ct_acc <- ct_fit_metrics[[1]]$overall["Accuracy"]
rf_acc <- rf_fit_metrics[[1]]$overall["Accuracy"]
lr_acc <- lr_fit_metrics[[1]]$overall["Accuracy"]
nb_acc <- nb_fit_metrics[[1]]$overall["Accuracy"]
svm_acc <- svm_fit_metrics[[1]]$overall["Accuracy"]
nn_acc <- nn_fit_metrics[[1]]$overall["Accuracy"]

#Plot Sensitivities
ct_Sens <- ct_fit_metrics[[1]]$byClass[, "Sensitivity"]
rf_Sens <- rf_fit_metrics[[1]]$byClass[, "Sensitivity"]
lr_Sens <- lr_fit_metrics[[1]]$byClass[, "Sensitivity"]
nb_Sens <- nb_fit_metrics[[1]]$byClass[, "Sensitivity"]
svm_Sens <- svm_fit_metrics[[1]]$byClass[, "Sensitivity"]
nn_Sens <- nn_fit_metrics[[1]]$byClass[, "Sensitivity"]

c(ct_Sens, rf_Sens, lr_Sens, nb_Sens, svm_Sens, nn_Sens)
c(rf_Sens, nn_Sens)

plot(x = 1:4, y = ct_Sens, ylim = c(0, 1), xlab = "Party", ylab = "Sensitivity",
     main = "Sensitivity Comparison Based on Validation Set",
     type = "b", col = "red", xaxt = "n")
axis(1, at = seq(1, 4, by = 1), labels = c("Conservative", "Labour", 
                                           "Other", "Scottish\nNational Party"))
points(x = 1:4, y = rf_Sens, type = "b", col = "purple", lty = 2)
points(x = 1:4, y = lr_Sens, type = "b", col = "yellow")
points(x = 1:4, y = nb_Sens, type = "b", col = "green")
points(x = 1:4, y = svm_Sens, type = "b", col = "blue")
points(x = 1:4, y = nn_Sens, type = "b", col = "pink", lty = 2)
legend("bottomleft", legend = c("Classification Tree",                                
                                "Random Forest",                                 
                                "Logistic Regression",
                                "Naive Bayes", 
                                "Support Vector Machine",  
                                "Neural Net"), 
       col = c("red", "purple", "yellow", "green", "blue", "pink"), 
       lwd = 2, cex = 0.75)

#Plot Specificity
ct_Spec <- ct_fit_metrics[[1]]$byClass[, "Specificity"]
rf_Spec <- rf_fit_metrics[[1]]$byClass[, "Specificity"]
lr_Spec <- lr_fit_metrics[[1]]$byClass[, "Specificity"]
nb_Spec <- nb_fit_metrics[[1]]$byClass[, "Specificity"]
svm_Spec <- svm_fit_metrics[[1]]$byClass[, "Specificity"]
nn_Spec <- nn_fit_metrics[[1]]$byClass[, "Specificity"]

plot(x = 1:4, y = ct_Spec, ylim = c(0, 1), xlab = "Party", ylab = "Specificity",
     main = "Specificity Comparison Based on Validation Set", lty = 2, 
     type = "b", col = "red", xaxt = "n")
axis(1, at = seq(1, 4, by = 1), labels = c("Conservative", "Labour", 
                                           "Other", "Scottish\nNational Party"))
points(x = 1:4, y = rf_Spec, type = "b", col = "purple", lty = 2)
points(x = 1:4, y = lr_Spec, type = "b", col = "yellow", lty = 2)
points(x = 1:4, y = nb_Spec, type = "b", col = "green", lty = 2)
points(x = 1:4, y = svm_Spec, type = "b", col = "blue", lty = 2)
points(x = 1:4, y = nn_Spec, type = "b", col = "pink", lty = 2)
legend("bottomleft", legend = c("Classification Tree",                                
                                "Random Forest",                                 
                                "Logistic Regression",
                                "Naive Bayes", 
                                "Support Vector Machine",  
                                "Neural Net"), 
       col = c("red", "purple", "yellow", "green", "blue", "pink"), 
       lwd = 2, cex = 0.75)

