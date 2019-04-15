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
library(randomForest) #For the random forest
library(e1071)        #For the support vector machine
library(neuralnet)    #For the neural net
library(pROC)         #For ROC curves 

# Set up the datasets -----------------------------------------------------
data.vote <- read.csv("data/Final Vote Data.csv", header = TRUE)
data.vote <- na.omit(data.vote)
data.vote$Constituency <- as.numeric(data.vote$Constituency)
head(data.vote)

#Split dataset into a training set (75%) and hold back (25%) for validation
set.seed(123)
ind <- sample(1:nrow(data.vote), 0.75 * nrow(data.vote), replace = FALSE) 
data.train <- data.vote[ind, -c(2, 4)]
data.validation <- data.vote[-ind,  -c(2, 4)] 

#Split training dataset into a training set (75%) and testing set (25%)
set.seed(123)
tuneind <- sample(1:nrow(data.train), 0.75 * nrow(data.train), replace = FALSE) 
tune.train <- data.train[tuneind, ]
tune.test <- data.train[-tuneind, ]

#Set up the cross validation to select the optimal parameters
set.seed(123)
fitControl <- trainControl(method = "cv", number = 5)

# DECISION TREE (Carlotta) -----------------------------------------------------
# Change levels into Yes, No, No Vote for nicer plots
data.vote.dt <- data.vote
data.vote.dt[, 6:13] <- data.vote.dt[, 6:13] %>% mutate_if(is.numeric, as.factor)
for(i in 6:13){ data.vote.dt[, i]  <- recode(data.vote.dt[, i], 
                                          "1" = "Yes", "0" = "No Vote", "-1" = "No")
}

data.train.dt <- data.vote.dt[ind, -c(2, 4)]
data.validation.dt <- data.vote.dt[-ind,  -c(2, 4)] 

#Step1: Begin with a small cp
tree <- rpart(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + 
                Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train.dt,
              control = rpart.control(cp = 0.01))

printcp(tree) #Compares trees with up to 5 splits
summary(tree)
rpart.plot(tree, type = 1, cex = 0.9)

#where cp is the complexity parameter. Any split that does not decrease the
#overall lack of fit by a factor of cp is not attempted. For instance, with
#anova splitting, this means that the overall R-squared must increase by cp at 
#each step. The main role of this parameter is to save computing time by pruning
#off splits that are obviously not worthwhile. Essentially,the user informs the
#program that any split which does not improve the fit by cp will likely be
#pruned off by cross-validation, and that hence the program need not pursue it.

#Pick the tree size that minimizes misclassification rate
#min prediction error
printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

#Prune the tree using the best cp.
tree.pruned <- prune(tree, cp = bestcp)
rpart.plot(tree.pruned)

printcp(tree.pruned) #Uses all five nodes

#Calculate predictions based on the validation dataset
dt.pred <- predict(tree.pruned, data.validation.dt[, 4:11])


#Calculate the MSE and Accuracy/Kappa
MSE <- mean((dt.pred - data.validation.dt$Constituency)^2)

dt.leave_remain_pred <- c()
for (i in 1:nrow(data.validation.dt)){
  if (dt.pred[i] >= 50) dt.leave_remain_pred[i] = "Remain"
  if (dt.pred[i] < 50) dt.leave_remain_pred[i] = "Leave"
}
dt.leave_remain_pred <- as.factor(dt.leave_remain_pred)

dt_fit_metrics <- vector("list", length(levels(data.validation.dt$LeaveRemain)))
for (i in seq_along(dt_fit_metrics)) {
  positive.class <- levels(data.validation.dt$LeaveRemain)[i]
  dt_fit_metrics[[i]] <- confusionMatrix(dt.leave_remain_pred, data.validation.dt$LeaveRemain,
                                         positive = positive.class)
}

# RANDOM FOREST (Lei) ----------------------------------------------------------
source("Scripts/RFFunction.R")
### create model for percentage prediction, regression random forest ###
originalreg.rf = randomForest(Constituency ~ Voting.1 + Voting.2 + Voting.3 + 
                                Voting.4 + Voting.5 + Voting.6 + Voting.7 + 
                                Voting.8, data = tune.train,
                              mtry = 2, ntree = 1000)
print(originalreg.rf)
plot(originalreg.rf)
tuneRF(x = tune.train[, 4:11], y = tune.train[, 1])

### Try to find out the best mtry value
findgoodmtryreg(tune.train[, 4:11], tune.train[, 1], 7, 123, tune.test)
### when mtry = 3, the MSE is minimum

## creat the new model, mtry = 3 and try to find the best ntree
modelreg.lr <- randomForest(Constituency ~ Voting.1 + Voting.2 + Voting.3 + 
                              Voting.4 + Voting.5 + Voting.6 + Voting.7 + 
                              Voting.8, mtry = 3, data = tune.train, ntree = 1100)
print(modelreg.lr)
plot(modelreg.lr)

## from the plot, we could know when ntree around 800 - 1000 is stable.
findgoodntreereg(tune.train[, 4:11], tune.train[, 5], 800, 1200, 50, 3, tune.test)
## when ntree = 950, mtry = 3, the MSE is minimum

## creat the best model of random forest for regression
modeltworeg.lr <- randomForest(Constituency ~ Voting.1 + Voting.2 + Voting.3 +
                                 Voting.4 + Voting.5 + Voting.6 + Voting.7 + 
                                 Voting.8, data = data.train, 
                               mtry = 3, ntree = 950)
print(modeltworeg.lr)
plot(modeltworeg.lr)

#Obtain predicitions based on validation set
rf.pred <- predict(modeltworeg.lr, data.validation)

#Calculate the MSE and Accuracy/Kappa
MSE <- mean((rf.pred - data.validation$Constituency)^2)

leave_remain_pred <- c()
for (i in 1:nrow(data.validation)){
  if (rf.pred[i] >= 50) leave_remain_pred[i] = "Remain"
  if (rf.pred[i] < 50) leave_remain_pred[i] = "Leave"
}
leave_remain_pred <- as.factor(leave_remain_pred)

rf_fit_metrics <- vector("list", length(levels(data.validation$LeaveRemain)))
for (i in seq_along(rf_fit_metrics)) {
  positive.class <- levels(data.validation$LeaveRemain)[i]
  rf_fit_metrics[[i]] <- confusionMatrix(leave_remain_pred, data.validation$LeaveRemain,
                                         positive = positive.class)
}

# GLM (Abtin) ------------------------------------------------------------------
# SUPPORT VECTOR MACHINE (José) ------------------------------------------------
svmfit <- svm(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train,
              kernel = "linear", cost = 10, scale = FALSE)

tune.out.svm <- tune(svm, Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                       Voting.5 + Voting.6 + Voting.7 + Voting.8, 
                     data = data.train, kernel = "linear",
                     ranges =list(cost=c(0.1,1,10,100),gamma=c(0.5,1,2,3,4)))
summary(tune.out.svm)

svmfit <- svm(Constituency ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                Voting.5 + Voting.6 + Voting.7 + Voting.8, data = data.train,
              kernel = "linear", cost = 0.1, gamma = 0.5,  scale = FALSE)

#Calculate predictions based on validation set
svm.pred <- predict(svmfit, data.validation[, 4:11])

#Calculate the MSE and accuracy
MSE <- mean((svm.pred - data.validation$Constituency)^2)

leave_remain_pred <- c()
for (i in 1:nrow(data.validation)){
  if (svm.pred[i] >= 50) leave_remain_pred[i] = "Remain"
  if (svm.pred[i] < 50) leave_remain_pred[i] = "Leave"
}
leave_remain_pred <- as.factor(leave_remain_pred)

svm_fit_metrics <- vector("list", length(levels(data.validation$LeaveRemain)))
for (i in seq_along(svm_fit_metrics)) {
  positive.class <- levels(data.validation$LeaveRemain)[i]
  svm_fit_metrics[[i]] <- confusionMatrix(leave_remain_pred, data.validation$LeaveRemain,
                                          positive = positive.class)
}

# NEURAL NET -------------------------------------------------------------------
nn_params <- train(Constituency ~ Voting.1 + Voting.2 + Voting.3 + 
                     Voting.4 + Voting.5 + Voting.6 + Voting.7 + 
                     Voting.8, data = data.train, 
                   method = "nnet", linear.output = TRUE,
                   trControl = fitControl)
nn_hidden <- nn_params$bestTune$size #Suggests 1 hidden layer
nn_decay <- nn_params$bestTune$decay

nn_model <- neuralnet(Constituency ~ Voting.1 + Voting.2 + Voting.3 + 
                        Voting.4 + Voting.5 + Voting.6 + Voting.7 + 
                        Voting.8, data = data.train,
                      stepmax = 1e+05, 
                      linear.output = TRUE, hidden = nn_hidden,
                      lifesign = "full")

plot(nn_model)

#Make predictions
nn_percent_preds  <- predict(nn_model, data.validation[, 4:11])
nn_mse <- mean((nn_percent_preds - data.validation$Constituency)^2)

#Change probabilities into classes
predclass <- c()
for (i in 1:nrow(data.validation)){
  if (nn_percent_preds[i] >= 50) predclass[i] = "Remain"
  if (nn_percent_preds[i] < 50) predclass[i] = "Leave"
}
predclass <- as.factor(predclass)

nn_fit_metrics <- vector("list", length(levels(data.validation$LeaveRemain)))
for (i in seq_along(nn_fit_metrics)) {
  positive.class <- levels(data.validation$LeaveRemain)[i]
  nn_fit_metrics[[i]] <- confusionMatrix(predclass, data.validation$LeaveRemain,
                                         positive = positive.class)
}

# COMPARING MODEL ACCURACY (Brooke) --------------------------------------------
#Obtain Kappas
dt_kappa <- dt_fit_metrics[[1]]$overall["Kappa"]
rf_kappa <- rf_fit_metrics[[1]]$overall["Kappa"]
#glm_kappa <- glm_fit_metrics[[1]]$overall["Kappa"]
svm_kappa <- svm_fit_metrics[[1]]$overall["Kappa"] #Appears to be the best kappa?
nn_kappa <- nn_fit_metrics[[1]]$overall["Kappa"]

list(DT = dt_kappa, RF = rf_kappa, SVM = svm_kappa, NN = nn_kappa)
#ROC Curve

dt.roc <- plot(roc(data.validation$LeaveRemain, order(dt.pred/100)),
               col="red", lwd = 2,  print.auc = TRUE, 
               print.auc.x = 1.1, print.auc.y = 1)
rf.roc <- plot(roc(data.validation$LeaveRemain, order(rf.pred/100)), 
               print.auc = TRUE, lwd = 2,
               col = "purple", add = TRUE, print.auc.x = 1.1, print.auc.y = 0.9)
# glm.roc <- plot(roc(data.validation$LeaveRemain, order(glm.pred/100)), 
#                print.auc = TRUE, lwd = 2,
#                col = "yellow", add = TRUE, print.auc.x = 1.1, print.auc.y = 0.8)
svm.roc <- plot(roc(data.validation$LeaveRemain, order(svm.pred/100)), 
                print.auc = TRUE, lwd = 2,
                col = "blue", add = TRUE, print.auc.x = 1.1, print.auc.y = 0.7)
# nn.roc <- plot(roc(data.validation$LeaveRemain, order(nn.pred/100)), 
#                print.auc = TRUE, lwd = 2,
#                col = "green", add = TRUE, print.auc.x = 1.1, print.auc.y = 0.6)
legend("bottomright", 
       legend = c("Decision Tree", "Random Forest", "Generalized Linear Model", 
                  "Support Vector Machine", "Neural Net"), cex = 0.75,
       col = c("red", "purple", "yellow", "blue", "green"), lwd = 2)
# 
# #Plot Sensitivities
# dt_Sens <- c(dt_fit_metrics[[1]]$byClass["Sensitivity"], dt_fit_metrics[[2]]$byClass["Sensitivity"])
# rf_Sens <- c(rf_fit_metrics[[1]]$byClass["Sensitivity"], rf_fit_metrics[[2]]$byClass["Sensitivity"])
# #glm_Sens <- c(glm_fit_metrics[[1]]$byClass["Sensitivity"], glm_fit_metrics[[2]]$byClass["Sensitivity"])
# svm_Sens <- c(svm_fit_metrics[[1]]$byClass["Sensitivity"], svm_fit_metrics[[2]]$byClass["Sensitivity"])
# nn_Sens <- c(svm_fit_metrics[[1]]$byClass["Sensitivity"], svm_fit_metrics[[2]]$byClass["Sensitivity"])
# 
# plot(x = 1:2, y = dt_Sens, ylim = c(0, 1), xlab = "Party", ylab = "Sensitivity",
#      main = "Sensitivity Comparison Based on Validation Set",
#      type = "b", col = "red", xaxt = "n")
# axis(1, at = c(1, 2), labels = c("Leave", "Remain"))
# points(x = 1:2, y = rf_Sens, type = "b", col = "purple", lty = 2)
# # points(x = 1:4, y = glm_Sens, type = "b", col = "yellow")
# points(x = 1:2, y = svm_Sens, type = "b", col = "blue")
# points(x = 1:2, y = nn_Sens, type = "b", col = "green", lty = 2)
# legend("bottomleft", legend = c("Classification Tree",                                
#                                 "Random Forest",                                 
#                                 "Generalised Linear Model",
#                                 "Support Vector Machine",  
#                                 "Neural Net"), 
#        col = c("red", "purple", "yellow", "blue", "green"), 
#        lwd = 2, cex = 0.75)
# 
# #Plot Specificity
# dt_Spec <- c(dt_fit_metrics[[1]]$byClass["Specificity"], dt_fit_metrics[[2]]$byClass["Specificity"])
# rf_Spec <- c(rf_fit_metrics[[1]]$byClass["Specificity"], rf_fit_metrics[[2]]$byClass["Specificity"])
# #glm_Spec <- c(glm_fit_metrics[[1]]$byClass["Specificity"], glm_fit_metrics[[2]]$byClass["Specificity"])
# svm_Spec <- c(svm_fit_metrics[[1]]$byClass["Specificity"], svm_fit_metrics[[2]]$byClass["Specificity"])
# nn_Spec <- c(svm_fit_metrics[[1]]$byClass["Specificity"], svm_fit_metrics[[2]]$byClass["Specificity"])
# 
# plot(x = 1:2, y = dt_Spec, ylim = c(0, 1), xlab = "Party", ylab = "Specificity",
#      main = "Specificity Comparison Based on Validation Set", lty = 2, 
#      type = "b", col = "red", xaxt = "n")
# axis(1, at = seq(1, 4, by = 1), labels = c("Conservative", "Labour", 
#                                            "Other", "Scottish\nNational Party"))
# points(x = 1:2, y = rf_Spec, type = "b", col = "purple", lty = 2)
# # points(x = 1:4, y = glm_Spec, type = "b", col = "yellow", lty = 2)
# points(x = 1:2, y = svm_Spec, type = "b", col = "blue", lty = 2)
# points(x = 1:2, y = nn_Spec, type = "b", col = "green", lty = 2)
# legend("bottomleft", legend = c("Classification Tree",                                
#                                 "Random Forest",                                 
#                                 "Logistic Regression",
#                                 "Support Vector Machine",  
#                                 "Neural Net"), 
#        col = c("red", "purple", "yellow", "blue", "green"), 
#        lwd = 2, cex = 0.75)
# 
# 
